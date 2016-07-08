(ns cmr.system-int-test.search.facets.collection-facets-v2-search-test
  "This tests retrieving v2 facets when searching for collections"
  (:require [clojure.test :refer :all]
            [cmr.system-int-test.search.facets.facets-util :as fu]
            [cmr.system-int-test.search.facets.facet-responses :as fr]
            [cmr.system-int-test.data2.collection :as dc]
            [cmr.system-int-test.utils.ingest-util :as ingest]
            [cmr.system-int-test.utils.search-util :as search]
            [cmr.system-int-test.utils.index-util :as index]
            [cmr.search.services.query-execution.facets.facets-v2-results-feature :as frf2]
            [cmr.common.mime-types :as mt]
            [cmr.common.util :refer [are3]]))

(use-fixtures :each (ingest/reset-fixture {"provguid1" "PROV1"}))

(def sk1 (dc/science-keyword {:category "Cat1"
                              :topic "Topic1"
                              :term "Term1"
                              :variable-level-1 "Level1-1"
                              :variable-level-2 "Level1-2"
                              :variable-level-3 "Level1-3"
                              :detailed-variable "Detail1"}))

(def sk2 (dc/science-keyword {:category "Hurricane"
                              :topic "Popular"
                              :term "Extreme"
                              :variable-level-1 "Level2-1"
                              :variable-level-2 "Level2-2"
                              :variable-level-3 "Level2-3"
                              :detailed-variable "UNIVERSAL"}))

(def sk3 (dc/science-keyword {:category "Hurricane"
                              :topic "Popular"
                              :term "UNIVERSAL"}))

(defn- search-and-return-v2-facets
  "Returns the facets returned by a search requesting v2 facets."
  ([]
   (search-and-return-v2-facets {}))
  ([search-params]
   (index/wait-until-indexed)
   (let [query-params (merge search-params {:page-size 0 :include-facets "v2"})]
     (get-in (search/find-concepts-json :collection query-params)
             [:results :facets]))))

(deftest all-facets-v2-test
  (fu/make-coll 1 "PROV1"
                (fu/science-keywords sk1 sk2)
                (fu/projects "proj1" "PROJ2")
                (fu/platforms fu/FROM_KMS 2 2 1)
                (fu/processing-level-id "PL1")
                {:organizations [(dc/org :archive-center "DOI/USGS/CMG/WHSC")]})
  (fu/make-coll 2 "PROV1"
                (fu/science-keywords sk1 sk3)
                (fu/projects "proj1" "PROJ2")
                (fu/platforms fu/FROM_KMS 2 2 1)
                (fu/processing-level-id "PL1")
                {:organizations [(dc/org :archive-center "DOI/USGS/CMG/WHSC")]})
  (is (= fr/expected-v2-facets-apply-links (search-and-return-v2-facets)))
  (testing "All fields applied for all facets"
    (let [search-params {:science-keywords-h {:0 {:category "Cat1"
                                                :topic "Topic1"
                                                :term "Term1"
                                                :variable-level-1 "Level1-1"
                                                :variable-level-2 "Level1-2"
                                                :variable-level-3 "Level1-3"}}
                         :project-h ["proj1"]
                         :platform-h ["DIADEM-1D"]
                         :instrument-h ["ATM"]
                         :processing-level-id-h ["PL1"]
                         :data-center-h "DOI/USGS/CMG/WHSC"}]
      (is (= fr/expected-v2-facets-remove-links (search-and-return-v2-facets search-params)))
      (testing "Some group fields not applied"
        (let [response (search-and-return-v2-facets
                        (dissoc search-params :platform-h :project-h :data-center-h))]
          (is (not (fu/applied? response :platform-h)))
          (is (not (fu/applied? response :project-h)))
          (is (not (fu/applied? response :data-center-h)))
          (is (fu/applied? response :science-keywords-h))
          (is (fu/applied? response :instrument-h))
          (is (fu/applied? response :processing-level-id-h)))))))

(def science-keywords-all-applied
  "Facet response with just the title, applied, and children fields. Used to verify that when
  searching for the deepest nested field (a value of Level1-3 for variable-level-3) all of the
  fields above variable-level-3 have applied set to true."
  {:title "Browse Collections",
   :children
   [{:title "Keywords", :applied true,
     :children
     [{:title "Cat1", :applied true,
       :children
       [{:title "Topic1", :applied true,
         :children
         [{:title "Term1", :applied true,
           :children
           [{:title "Level1-1", :applied true,
             :children
             [{:title "Level1-2", :applied true,
               :children
               [{:title "Level1-3", :applied true}]}]}]}]}]}]}]})

(deftest facet-v2-sorting
  ;; 55 platforms all with the same count (2) and default priority
  (fu/make-coll 1 "PROV1" (fu/platforms "default" 55))
  (fu/make-coll 2 "PROV1" (fu/platforms "default" 55))
  ;; 1 platform with a count of 1 but high priority, so it should appear
  (fu/make-coll 3 "PROV1" {:platforms [(dc/platform {:short-name "Terra"})]})
  ;; 55 platforms with a count of 1, none of which should appear
  (fu/make-coll 4 "PROV1" (fu/platforms "low" 55))

  (let [response (search-and-return-v2-facets)]
    (testing "high priority items appear regardless of count"
      (is (fu/facet-included? response :platform-h "Terra")))

    (testing "same-priority items appear based on highest count"
      (is (not-any? #(= "low" (subs % 0 3)) (fu/facet-values response :platform-h))))

    (testing "items are sorted alphabetically"
      (let [index (partial fu/facet-index response :platform-h)]
        (is (< (index "default-p2") (index "default-p10")))
        (is (< (index "default-p0") (index "Terra")))))))

(def partial-science-keywords-applied
  "Facet response with just the title, applied, and children fields. Used to verify that when
  searching for a nested field (a value of TERM1 for term) all of the fields above term have
  applied set to true and any fields below have applied set to false. Also only one level below
  the last applied term is returned. In the case of searching for a term, only variable-level-1
  should be returned. Both variable-level-2 and variable-level-3 should be omitted from the
  response."
  {:title "Browse Collections",
   :children
   [{:title "Keywords", :applied true,
     :children
     [{:title "Cat1", :applied true,
       :children
       [{:title "Topic1", :applied true,
         :children
         [{:title "Term1", :applied true,
           :children
           [{:title "Level1-1", :applied false}]}]}]}]}]})

(deftest hierarchical-applied-test
  (fu/make-coll 1 "PROV1" (fu/science-keywords sk1))
  (testing "Children science keywords applied causes parent fields to be marked as applied"
    (are3 [search-params expected-response]
      (is (= expected-response (fu/prune-facet-response (search-and-return-v2-facets search-params)
                                                        [:title :applied])))

      "Lowest level field causes all fields above to be applied."
      {:science-keywords-h {:0 {:variable-level-3 "Level1-3"}}}
      science-keywords-all-applied

      "Middle level field causes all fields above to be applied, but not fields below."
      {:science-keywords-h {:0 {:term "Term1"}}}
      partial-science-keywords-applied)))

(deftest remove-facets-without-collections
  (fu/make-coll 1 "PROV1" (fu/platforms "ASTER" 1))
  (fu/make-coll 1 "PROV1" (fu/platforms "MODIS" 1))
  (testing (str "When searching against faceted fields which do not match any matching collections,"
                " a link should be provided so that the user can remove the term from their search.")
    (let [search-params {:science-keywords-h {:0 {:category "Cat1"
                                                  :topic "Topic1"
                                                  :term "Term1"
                                                  :variable-level-1 "Level1-1"
                                                  :variable-level-2 "Level1-2"
                                                  :variable-level-3 "Level1-3"}}
                         :project-h ["proj1"]
                         :platform-h ["ASTER-p0"]
                         :instrument-h ["ATM"]
                         :processing-level-id-h ["PL1"]
                         :data-center-h "DOI/USGS/CMG/WHSC"
                         :keyword "MODIS"}
          response (search-and-return-v2-facets search-params)]
      (is (= fr/expected-facets-with-no-matching-collections response))))
  (testing "Facets with multiple facets applied, some with matching collections, some without"
    (is (= fr/expected-facets-modis-and-aster-no-results-found
           (search-and-return-v2-facets {:platform-h ["moDIS-p0", "ASTER-p0"]
                                         :keyword "MODIS"})))))

(defn- get-lowest-hierarchical-depth
  "Returns the lowest hierachical depth within the facet response for any hierarchical fields."
  ([facet]
   (get-lowest-hierarchical-depth facet -1))
  ([facet current-depth]
   (apply max
          current-depth
          (map #(get-lowest-hierarchical-depth % (inc current-depth))
              (:children facet)))))

(deftest appropriate-hierarchical-depth
  (fu/make-coll 1 "PROV1" (fu/science-keywords sk1 sk2))
  (testing "Default to 2 levels without any search parameters"
    (is (= 2 (get-lowest-hierarchical-depth (search-and-return-v2-facets {})))))
  (are [sk-param expected-depth]
    (= expected-depth (get-lowest-hierarchical-depth (search-and-return-v2-facets
                                                      {:science-keywords-h {:0 sk-param}})))

    {:category "Cat1"} 2
    {:topic "Topic1"} 3
    {:topic "Topic1" :category "Cat1"} 3
    {:term "Term1"} 4
    {:term "Term1" :category "Cat1"} 4
    {:term "Term1" :category "Cat1" :topic "Topic1"} 4
    {:variable-level-1 "Level1-1"} 5
    {:variable-level-1 "Level1-1" :term "Term1" :category "Cat1" :topic "Topic1"} 5
    {:variable-level-2 "Level1-2" :term "Term1" :category "Cat1" :topic "Topic1"
     :variable-level-1 "Level1-1"} 6
    {:variable-level-3 "Level1-3"} 6
    {:variable-level-3 "Level1-3" :variable-level-2 "Level1-2" :term "Term1" :category "Cat1"
     :topic "Topic1" :variable-level-1 "Level1-1"} 6))

(deftest empty-hierarchical-facets-test
  (let [expected-empty-facets {:title "Browse Collections"
                               :type "group"
                               :has_children false}]
    (is (= expected-empty-facets (search-and-return-v2-facets)))))

(deftest some-facets-missing-test
  (fu/make-coll 1 "PROV1"
                (fu/science-keywords sk3 sk2)
                (fu/processing-level-id "PL1"))
  (is (= fr/partial-v2-facets (search-and-return-v2-facets))))

(deftest invalid-facets-v2-response-formats
  (testing "invalid xml response formats"
    (are [resp-format]
         (= {:status 400 :errors ["V2 facets are only supported in the JSON format."]}
            (search/get-search-failure-xml-data
              (search/find-concepts-in-format resp-format :collection {:include-facets "v2"})))
         mt/echo10
         mt/dif
         mt/dif10
         mt/xml
         mt/kml
         mt/atom
         mt/iso19115))

  (testing "invalid json response formats"
     (are [resp-format]
         (= {:status 400 :errors ["V2 facets are only supported in the JSON format."]}
            (search/get-search-failure-data
              (search/find-concepts-in-format resp-format :collection {:include-facets "v2"})))
         mt/umm-json
         mt/opendata)))
