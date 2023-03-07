(ns cmr.indexer.data.concepts.generic-util
  "Contains functions to parse and convert Generic Documents (that is a document
   complying to a schema supported by the Generic Document system) to and object
   that can be indexed in lucine."
  (:require
   [clojure.string :as string]
   [cmr.common.log :refer (debug info warn error)]))

(defn jq->list
  "To make configuration authoring simple for humans, fields of a JSON record are
   to be denoted using a syntax similar to the jq unix command. This syntax will
   define a path into a nested set of fields. The jq path is passed in to this
   function and will be converted to a list that can be used with the built in
   clojure function get-in to retrieve JSON field content.
   Example: .Level1.Level2[1].Level3 -> [:Level1 :Level2 1 :Level3]"
  ([jq-path] (jq->list jq-path str))
  ([jq-path namer]
   (into [] (map (fn
                   [value]
                   (if (every? #(Character/isDigit %) value)
                     (Integer/parseInt value)
                     (namer value)))
                 (-> jq-path
                     (string/replace #"^\." "")
                     (string/replace #"\[(\d+)\]" ".$1")
                     (string/split #"\."))))))
