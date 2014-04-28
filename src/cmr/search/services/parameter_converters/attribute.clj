(ns cmr.search.services.parameter-converters.attribute
  "Contains functions for converting additional attribute search parameters to a query model"
  (:require [cmr.search.models.query :as qm]
            [cmr.search.services.parameters :as p]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [cmr.search.services.messages.attribute-messages :as msg]
            [cmr.common.services.errors :as errors]
            [cmr.common.date-time-parser :as date-time-parser])
  (:import [cmr.search.models.query
            AttributeValueCondition
            AttributeRangeCondition]
           clojure.lang.ExceptionInfo))

(defn empty->nil
  "Converts an empty string to nil"
  [s]
  (when-not (empty? s)
    s))


(defn value->condition
  "Parses an additional attribute value into it's constituent parts.
  Values must be comma separated in one of the following two formats

    * type,name,value
      * Example: \"string,fav_color,blue\"
    * type,name,min,max
      * Example: \"float,cloud_cover_range,0,100\"
      * Example: \"float,cloud_cover_range,,80\"  means must be less than 80 with no lower bounds
      * Example: \"float,cloud_cover_range,10,\"  means must be greater than 10 with no upper bounds"
  [value]
  (let [comma-escape "\\,"
        comma-replace "%COMMA%" ; used to replace escaped commas during splitting
        parts (-> value
                  (str/replace comma-escape comma-replace)
                  (str/split #"," 5))
        parts (map #(str/replace % comma-replace ",") parts)
        parts (map empty->nil parts)]

    (case (count parts)
      3
      (let [[t n v] parts]
        (if n
          (qm/map->AttributeValueCondition
            {:type t
             :name n
             :value v})
          {:errors [(msg/invalid-name-msg n)]}))
      4
      (let [[t n minv maxv] parts]
        (if n
          (qm/map->AttributeRangeCondition
            {:type t
             :name n
             :min-value minv
             :max-value maxv})
          {:errors [(msg/invalid-name-msg n)]}))

      ;; else
      {:errors [(msg/invalid-num-parts-msg)]})))

(def attribute-type->parser-fn
  "A map of attribute types to functions that can parse a value"
  {:string identity
   :float #(Double/parseDouble %)
   :int #(Integer/parseInt %)
   :datetime date-time-parser/parse-datetime
   :time date-time-parser/parse-time
   :date date-time-parser/parse-date})

(defmulti parse-condition-values
  "Parses the component type into their expected values"
  (fn [condition]
    (type condition)))

(defn parse-field
  "Attempts to parse the given field and update the condition. If there are problems parsing an
  errors attribute will be returned."
  [condition field parser type]
  (let [handle-exception #(update-in condition [:errors]
                                     conj (msg/invalid-value-msg type (get condition field)))]
    (try
      (update-in condition [field] parser)
      (catch NumberFormatException e
        (handle-exception))
      (catch ExceptionInfo e
        (handle-exception)))))

(defmethod parse-condition-values AttributeRangeCondition
  [condition]
  (let [{:keys [type min-value max-value]} condition]
    (if (or min-value max-value)
      (let [parser (attribute-type->parser-fn type)
            parser #(when % (parser %))
            condition (-> condition
                          (parse-field :min-value parser type)
                          (parse-field :max-value parser type))]
        (if (:errors condition)
          {:errors (:errors condition)}
          condition))
      {:errors [(msg/one-of-min-max-msg)]})))

(defmethod parse-condition-values AttributeValueCondition
  [condition]
  (let [{:keys [type value]} condition]
    (if (:value condition)
      (let [parser (attribute-type->parser-fn type)
            condition (parse-field condition :value parser type)]
        (if (:errors condition)
          {:errors (:errors condition)}
          condition))
      {:errors [(msg/invalid-value-msg type value)]})))

(defn parse-component-type
  "Parses the type and it's values"
  [condition]
  (if-let [type (some (set qm/attribute-types) [(keyword (:type condition))])]
    (parse-condition-values (assoc condition :type type))
    {:errors [(msg/invalid-type-msg (:type condition))]}))

(defn parse-value
  "Parses an additional attribute value into it's constituent parts"
  [value]
  (let [condition (value->condition value)]
    (if (:errors condition)
      condition
      (parse-component-type condition))))

;; Converts parameter and values into collection query condition
(defmethod p/parameter->condition :attribute
  [concept-type param values options]

  (let [conditions (map parse-value values)
        failed-conditions (seq (filter :errors conditions))]
    (if failed-conditions
      (errors/internal-error!
        (format
          "Found invalid value that should have been validated already. Values: %s"
          (pr-str values)))

      ;; TODO we'll or conditions for now. We have to make this selectable later.
      (qm/or-conds conditions))))
