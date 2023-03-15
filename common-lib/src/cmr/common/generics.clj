(ns cmr.common.generics
  "Defines utilities for new generic document pipeline. Most functions will deal
   with either returning generic config files, or lists of approved generics."
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [cmr.common.config :as cfg]
   [camel-snake-kebab.core :as csk]
   [cmr.common.log :as log :refer [error, info]]
   [cmr.common.util :as util]
   [cmr.schema-validation.json-schema :as js-validater]
   [inflections.core :as inf]))

(defn approved-generic?
  "Check to see if a requested generic is on the approved list.
   Parameters:
   * schema: schema keyword like :grid
   * version: string like 0.0.1
   Returns: true if schema and version are supported, nil otherwise"
  [schema version]
  (when (and schema version)
    (some #(= version %) (schema (cfg/approved-pipeline-documents)))))

(defn latest-approved-documents
  "Return a map of all the configured approved generics and the latest version
   string for each one.
   Return {:doc-type \"1.2.3\"}"
  []
  (reduce (fn [data item]
            (assoc data (first item) (last (second item))))
          {}
          (cfg/approved-pipeline-documents)))

(def documents-all-versions
  "Return the list of all versions of the generic documents in the system"
  (cfg/approved-pipeline-documents))

(defn current-generic-version
  "The current/latest version of the generic UMM schema
   similar to umm-spec-lib/src/cmr/umm_spec/versioning.clj.
   Parameters:
   * generic-keyword: [:grid | ...]
   Returns: string"
  [concept-type]
  (-> documents-all-versions concept-type last))

(defn latest-approved-document-types
  "Return a list of configured approved generic keywords
   Returns: (:grid :data-quality-summary ...)"
  []
  (keys (latest-approved-documents)))

(defn read-schema-file
  "Return the specific schema given the schema keyword name and version number.
   Throw an error if the file can't be read.
   Parameters:
   * file-name: [metadata | index | schema]
   * generic-keyword: [:grid | ...]
   * generic-version: 0.0.1
   Returns: string"
  [file-name generic-keyword generic-version]
  (try
    (-> "schemas/%s/v%s/%s.json"
        (format (name generic-keyword) generic-version (name file-name))
        (io/resource)
        (slurp))
    (catch Exception e
      (error
       (format (str "The %s.json file for schema [%s] version [%s] cannot be found. "
                    " - [%s] - "
                    "Please make sure that it exists. %s")
               (name file-name)
               (name generic-keyword)
               generic-version
               (format "schemas/%s/v%s/%s.json" (name generic-keyword) generic-version (name file-name))
               (.getMessage e))))))

(defn read-schema-index
  "Return the schema index configuration file given the schema name and version
   number. Throw an error if the file can't be read.
   Parameters:
   * generic-keyword: [:grid | ...]
   * generic-version: 0.0.1
   Returns: string"
  [generic-keyword generic-version]
  (read-schema-file "index" generic-keyword generic-version))

(defn read-schema-specification
  "Return the schema specification file given the schema name and version number.
   Throw an error if the file can't be read.
   Parameters:
   * generic-keyword: [:grid | ...]
   * generic-version: 0.0.1
   Returns: string"
  [generic-keyword generic-version]
  (read-schema-file "schema" generic-keyword generic-version))

(defn read-schema-example
  "Return the schema example metadata file given the schema name and version
   number. Throw an error if the file can't be read.
   Parameters:
   * generic-keyword: [:grid | ...]
   * generic-version: 0.0.1
   Returns: string"
  [generic-keyword generic-version]
  (read-schema-file "metadata" generic-keyword generic-version))

(defn validate-index-against-schema
  "Validate a document, returns an array of errors if there are problems
   Parameters:
   * raw-json, json as a string to validate
   Returns: list of errors or nil"
  [raw-json]
  (let [schema-file (read-schema-file :schema :index "0.0.1")
        schema-obj (js-validater/json-string->json-schema schema-file)]
    (js-validater/validate-json schema-obj raw-json)))

(defn approved-generic-concept-prefixes
  "Return the active list of approved generic content types with the defined
   prefix in the :SubConceptType field found in the index.json file. If field is
   not defined, then X is used.
   Parameters: none, based off approved-documents?
   Return: {doc-type \"concept-prefix\"}"
  []
  (reduce (fn [data item]
            (let [generic-keyword (first item)
                  index-raw (read-schema-index generic-keyword (second item))
                  parse-errors (validate-index-against-schema index-raw)]
              (when-not (some? parse-errors)
                (assoc data
                       generic-keyword
                       (get (json/parse-string index-raw true) :SubConceptType "X")))))
          {}
          (latest-approved-documents)))

(def generic-concept-types-reg-ex
  "Creates a regular expression for all of the generic concepts. Used to create API endpoints."
  (->> (latest-approved-document-types)
       (map name)
       (string/join "|")))

(def plural-generic-concept-types-reg-ex
  "Creates a pluralized regular expression for all of the generic concepts. Used to create API endpoints."
  (->> (latest-approved-document-types)
       (map inf/plural)
       (string/join "|")))

(defn retrieve-custom-generic-search-parameters
  "Retrieve the array of indexes that a particular generic concept has"
  [concept-type]
  (get (json/parse-string
        (read-schema-index concept-type
                           (current-generic-version concept-type)) true) :Indexes
       (str "Missing Indexes field in the index.json for " concept-type)))

(defn only-elastic-preferences
  "Go through all the index configurations and return only the ones related to 
   generating elastic values. If an index does not specify what type it is for,
   then assume elastic"
  [list-of-indexes]
  (keep #(if (not (nil? %)) %)
        (map
         (fn [x] (when (or (nil? (:Type x)) (= "elastic" (:Type x))) x))
         list-of-indexes)))



(defn only-specified-type-preferences
  "Go through all the index configurations in a generic index.json and return only the ones related to 
   generating elastic values. If an index does not specify what type it is for,
   then assume elastic"
  ([list-of-indexes]
   (only-specified-type-preferences list-of-indexes "elastic"))
  ([list-of-indexes type-key]
   (vec (keep #(if (not (nil? %)) %)
              (map
               (fn [x] (when (= type-key (:Type x)) x))
               list-of-indexes)))))

(defn format-search-parameter-keys
  "Return the list of all of the keys that are indexed by elastic search 
   lower case them to match how they are stored in elastic-search"
  [concept-type]
  (map #(keyword (string/lower-case (:Name %))) (only-elastic-preferences (retrieve-custom-generic-search-parameters concept-type))))

(defn generic-search-parameter-keys
  "Return the list of keys in each generics index.json file without formatting"
  [concept-type]
  (map #(keyword (:Name %)) (only-elastic-preferences (retrieve-custom-generic-search-parameters concept-type))))

;; TODO: Remove me common-params/param-mappings
;; (defn generic-custom-param-mappingss
;;   "Retrieve the generics index as well the type of mapping that it is string etc"
;;   [concept-type]
;;   (into {}  (vec (map
;;                   #(vec [(keyword (string/lower-case (:Name %)))
;;                          (keyword (:Mapping %))])
;;                   (only-elastic-preferences (retrieve-custom-generic-search-parameters concept-type))))))

;; This is using the new style

(defn generic-custom-param-mappings-no-formatting
  "Retrieve the generics index as well the type of mapping that it is string etc"
  [context-object]
  ;; Dig into the configuration and pull out the mapping for that particular one
   (map #(keyword (:Name %)) context-object))




(defn generic-custom-param-mappings-type
  "Retrieve the generics index as well the type of mapping that it is string etc"
  [context-object]
  ;; Dig into the configuration and pull out the mapping for that particular one
  (into {} (map #(vec [(keyword (string/lower-case (:Name %))) (keyword (string/lower-case (:Mapping (:Configuration %))))]) context-object)))



(defn generic-custom-param-mappings-for-reassign-index
  "Retrieve the generics index as well the type of mapping that it is string etc"
  [context-object]
  (into {} (map #(vec [(keyword (string/lower-case (:Name %))) (keyword (string/lower-case (:Field %)))]) context-object)))




(defn retrieve-complex-index-fields
  "Return the keys for a specified generic which are indexed as complex-fields i.e. Index: complex-field"
  [concept-type]
  (remove nil? (map #(keyword (if (= "complex-field" (:Indexer %)) (:Name %)))
                    (only-elastic-preferences (retrieve-custom-generic-search-parameters concept-type)))))


;; This will return empty for concepts that do NOT have any custom parameters
(defn retrieve-custom-search-parameter-mappings
  [concept-type-key]
  (assoc {} concept-type-key (only-specified-type-preferences (retrieve-custom-generic-search-parameters concept-type-key) "search")))

;; (def return-big-search-param-map
;;   (doseq [i (latest-approved-document-types)] (println (conj {} {(keyword i) (retrieve-custom-search-parameter-mappings i)})))
;;   )

(def return-big-search-param-map
  (into {} (map #(retrieve-custom-search-parameter-mappings %)  (latest-approved-document-types))))



(comment
  (into {}  (vec (map
                  #(vec [(keyword (string/lower-case (:Name %)))
                         (keyword (:Mapping %))])
                  (only-elastic-preferences (retrieve-custom-generic-search-parameters :grid)))))

  (map #((keyword (string/lower-case (:Name %))) :random) (only-elastic-preferences (retrieve-custom-generic-search-parameters :grid)))

  (map #((:Name %) (:Mapping %)) (only-elastic-preferences (retrieve-custom-generic-search-parameters :grid)))


  (only-specified-type-preferences (retrieve-custom-generic-search-parameters :grid) "search"))

;; New comments
(comment
  (format-search-parameter-keys :grid))



