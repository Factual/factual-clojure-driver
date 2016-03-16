(ns factual.test.api
  (:require [factual.api :as fact]
            [sosueme.conf :as conf])
  (:import [factual.api factual-error])
  (:use [clojure.test]
        [slingshot.slingshot]))

(defn connect
  "Test fixture that connects this namespace to Factual's API.
   You must put your key and secret in ~/.factual/factual-auth.yaml, which should
   look like:

   ---
   key: MYKEY
   secret: MYSECRET"
  [f]
  (let [{:keys [key secret]} (conf/dot-factual "factual-auth.yaml")]
    (fact/factual! key secret))
  (f))

(use-fixtures :once connect)

(deftest test-fetch-random-sample
  (is (< 10 (count (fact/fetch {:table :places})))))

(deftest test-fetch-filters
  (let [res (fact/fetch {:table :restaurants-us :filters {:name "Starbucks"}})
        uniq-names (vec (distinct (map #(get % "name") res)))]
    (is (= ["Starbucks"] uniq-names))))

(deftest test-fetch-factual-error
  (try+
    (fact/fetch {:table :places
                :filters {:factual_id "97598010-433f-4946-8fd5-4a6dd1639d77" :BAD :PARAM}})
    (catch factual-error {code :code message :message opts :opts}
     (is (not (nil? code)))
     (is (not (nil? message)))
     (is (not (nil? opts)))
     (is (= (get-in opts [:filters :BAD]) :PARAM)))))

(deftest test-fetch-nearby-cafes
  "Returns up to 50 cafes within specified miles of specified location."
  []
  (let [res (fact/fetch
              {:table :places
               :q "cafe"
               :geo {:$circle {:$center [34.06018 -118.41835]
                               :$meters (* 3 1609.344)}}
               :include_count true
               :limit 5})]
    (is (= 5 (count res)))
    (is (< 5 (get-in (meta res) [:response :total_row_count])))))

(deftest test-fetch-row
  (let [res (first (fact/fetch-row :places "03d401b7-e4f3-4216-b1c9-5bb08be3d786"))]
    (is (= "03d401b7-e4f3-4216-b1c9-5bb08be3d786" (res "factual_id")))))

(deftest test-resolve
  (let [res (first
             (fact/resolve {:name "los angeles international airport"
                           :address "1 World Way, Los Angeles, CA 90045"}))]
    (is (= true (res "resolved")))))

(deftest test-match
  (let [res (fact/match {:name "McDonalds" :latitude 34.05671 :longitude -118.42586})]
    (is (= 1 (count res)))
    (is (contains? (first res) "factual_id"))))

(deftest test-crosswalk
  (is (< 3 (count
             (fact/fetch {:table :crosswalk-us :filters {:factual_id "3b9e2b46-4961-4a31-b90a-b5e0aed2a45e"}})))))

(deftest test-multi
  (let [res (fact/multi {"q1" {:api fact/fetch* :args [{:table :global :q "cafe" :limit 10}]}
                         "q2" {:api fact/facets* :args [{:table :global :select "locality,region" :q "http://www.starbucks.com"}]}})]
    (is (not (empty? (get-in res ["q2" "locality"]))))
    (is (= 10 (count (res "q1"))))))

(defn every-locality? [res val]
  (every? #(= % val) (map #(get % "locality") res)))

(deftest test-unicode-basic
  (let [res (fact/fetch {:table :global
                         :filters {:locality "大阪市"}
                         :limit 5})]
    (is (= (count res) 5))
    (is (every-locality? res "大阪市"))))

(deftest test-unicode-multi
         (let [mres (fact/multi {"q1" {:api fact/fetch* :args [{:table :global :filters {:locality "בית שמש"}}]}
                                 "q2" {:api fact/fetch* :args [{:table :global :filters {:locality "München"} :limit 10}]}})
               res1 (mres "q1")
               res2 (mres "q2")]

    (is (> (count res1) 0))
    (is (every-locality? res1 "בית שמש"))

    (is (= (count res2) 10))
    (is (every-locality? res2 "München"))))
