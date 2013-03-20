(ns factual.api
  (:refer-clojure :exclude [resolve])
  (:require
    [clojure.java.io :as io]
    [oauth.v1 :as oauth]
    [cheshire.core :as json]
    [clojure.walk :as walk]
    [clj-http.client :refer [generate-query-string]]
    [clj-http.util :refer [url-decode]]
    [slingshot.slingshot :refer [throw+]]
    [clojure.java.io :refer [reader]])
  (:import
   [java.io InputStream]))

;;;

(def ^:private DRIVER_VERSION_TAG "factual-clojure-driver-v1.4.4")

(def ^:private ^:dynamic consumer-atom (atom nil))

(defrecord factual-error [code message opts])

(def ^:dynamic *debug* false)

(def ^:dynamic *base-url* "http://api.v3.factual.com/")

(defn factual!
  "Sets your authentication with the Factual service.
   'key' is your Factual API key
   'secret' is your Factual API secret"
  [key secret]
  (reset! consumer-atom (oauth/make-consumer :oauth-consumer-key key :oauth-consumer-secret secret)))

(defn consumer []
  (if-let [consumer @consumer-atom]
    consumer
    (throw (IllegalArgumentException. "The Factual driver must be initialized using 'factual.api/factual!'"))))

(defn service!
  "Sets this driver to use the specified base service URL.
   This could be handy for testing purposes, or if Factual is
   supporting a custom service for you. Example usage:

     (service! \"http://api.dev.cloud.factual\")

   Don't forget the leading http:// and don't forget the trailing /
   in the base url you supply."
  [base]
  (def ^:dynamic *base-url* base))

(defmacro with-service
  "Allows temporary use of a different base service URL, within the
   scope of body. This could be handy for testing purposes, or if
   Factual is supporting a custom service for you. Example usage:

     (with-service \"http://api.dev.cloud.factual.com/\"
       (fetch :places))

   Don't forget the leading http:// and don't forget the trailing /
   in the base url you supply."
  [base & body]
  `(binding [*base-url* ~base]
     ~@body))

(defmacro debug
  [& body]
  `(binding [*debug* true]
     (time ~@body)))

;;;

(defn- meta? [x]
  (instance? clojure.lang.IMeta x))

(defn- extract-meta [res]
  (if-let [data (or
                  ;; standard result set
                  (get-in res [:response :data])
                  ;; schema result
                  (get-in res [:response :view :fields])
                  ;; submit result
                  (get-in res [:response]))]
    (if-not (meta? data)
      data
      (with-meta data
        (merge
          (dissoc res :response)
          {:response (dissoc (:response res) :data)})))
    res))

(defn- json-params [m]
  (reduce
   (fn [m [k v]]
     (assoc m
       ;; query param name
       (name k)
       ;; query param value
       (if (or (keyword? v) (string? v))
            (name v)
            (json/generate-string v))))
   {} m))

(defn- debug-rsp [resp]
  (println "--- response debug ---")
  (let [metadata (meta resp)
        status (:status metadata)
        hdrs (:headers metadata)]
    (println "resp status code:" status)
    (println "resp headers:")
    (clojure.pprint/pprint (into {} hdrs))
    (println "resp:")
    (println resp)
    (println "----------------------")))

(defn- debug-req [req]
  (println "request parameters:")
  (clojure.pprint/pprint (:params req))
  (println "body form parameters:")
  (clojure.pprint/pprint (:content req)))

(defn- stringify-keys [x]
  (walk/postwalk
    #(if (keyword? %)
       (name %)
       %)
    x))

(defn execute-request
  "Executes the specified request and returns the results.

   The incoming argument must be a map representing the request.

   The returned results will have metadata associated with it,
   built from the results metadata returned by Factual.

   In the case of a bad response code, throws a factual-error record
   as a slingshot stone. The record will include any opts that were
   passed in by user code."
  [{:keys [method path params content raw-request response-transform]
    :or {method :get
         response-transform stringify-keys}
    :as req}]

  (let [url (str *base-url* path)
        headers {"X-Factual-Lib" DRIVER_VERSION_TAG}
        req (merge
              {:method method
               :url url
               :headers headers
               :query-params (if params (json-params params) nil)
               :body (if content (generate-query-string (json-params content)) nil)
               :throw-exceptions false
               :as :json
               :save-request? true
               :debug *debug*
               :debug-body *debug*}
              raw-request)

        _ (when *debug* (debug-req req))
        rsp ((consumer) req)
        _ (when *debug* (debug-rsp rsp))

        status (:status (meta rsp))
        valid-response? (or (instance? InputStream rsp) (= 200 status))]

    (when-not valid-response?
      (throw+ (factual-error. status (:message rsp) params)))

    (let [response-transform (fn [x]
                               (let [m (meta x)
                                     x (response-transform x)]
                                 (if (meta? x)
                                   (with-meta x m)
                                   x)))]
      (response-transform
        (if (.equalsIgnoreCase path "multi")
          (into {} (for [[k v] rsp] [k (extract-meta v)]))
          (extract-meta rsp))))))

;;;

(defn- fetch-disp
  "Dispatch method for fetch. Returns:
   :q if argument is a query map
   :table if argument is just the table name
   :table-and-q if 2 arguments, expected to be table name and query map"
  [& args]
  (if (= 1 (count args))
    (if (map? (first args))
      :q
      :table)
    :table-and-q))

(defn fetch*
  "Returns a query for fetch requests, which can be passed into 'execute-request' or used within 'multi'."
  [m]
  {:pre [(:table m)]}
  
  {:path (str "t/" (name (:table m))) :params (dissoc m :table) })

(defmulti fetch
  "Runs a fetch request against Factual and returns the results.

   Supports 3 variations depending on the arguments you pass in:

   Variation 1: [q]
   q is a hash-map specifying the full query. The only required
   entry is :table, which must be associated with valid Factual
   table name. Optional query parameters, such as row filters and geolocation
   queries, are specified with further entries in q. Example usages:

     (fetch {:table :global})
     (fetch {:table :places :q \"cafe\"})

  Variation 2: [table q]
  table is the name of a valid Factual table.
  q is a hash-map specifying the full query, such as row filters and geolocation
  queries. Example usage:

     (fetch :places {:q \"cafe\" :limit 10})

  Variation 3: [table]
  table is the name of a valid Factual table. This is a very limited call, since
  it does not support any query parameters and will therefore just return a
  random sample of results from the specified table. Example usage:

     (fetch :places)"
  fetch-disp)

(defmethod fetch :q
  [q]
  (execute-request (fetch* q)))

(defmethod fetch :table
  [table]
  (fetch {:table table}))

(defmethod fetch :table-and-q
  [table q]
  (fetch (assoc q :table table)))

;;;

(defn- facets-disp
  "Dispatch method for facets. Returns:
   :q if argument is a query map
   :table-and-select if table name and select(s) are specified
   :table-and-q if table name and query options are specified"
  [& args]
  (if (= 1 (count args))
    :q
    (if (map? (second args))
      :table-and-q
      :table-and-select)))

(defn facets*
  "Returns a query for facet requests, which can be passed into 'execute-request' or used within 'multi'."
  [q]
  {:pre [(:table q) (:select q)]}
  
  {:path (str "t/" (name (:table q)) "/facets") :params (dissoc q :table)})

(defmulti facets
  "Runs a Facets request against Factual and returns the results.

   Supports 3 variations depending on the arguments you pass in:

   Variation 1: [q]
   q is a hash-map specifying the full query, which can include things like row filters and
   geolocation filtering. Required entries:
     :table   the name of a valid Factual table, e.g. :places
     :select  the field(s) to Facet as a comma-delimted string, e.g. \"locality,region\"
   Example usages:

     (facets {:table :global :select \"locality\"})
     (facets {:table :places :select \"locality,region\" :q \"starbucks\"})

   Variation 2: [table q]
   table is the name of a valid Factual table.
   q is a hash-map specifying the full query, which can include things like row filters and
   geolocation filtering. Required entry:
     :select  the field(s) to Facet as a comma-delimted string, e.g. \"locality,region\"
   Example usages:

     (facets :restaurants-us {:select \"region\" :limit 50})
     (facets :global {:select \"locality,region\" :q \"starbucks\"})

   Variation 3: [table select]
   table is the name of a valid Factual table.
   select is the field(s) to Facet as a comma-delimted string, e.g. \"locality,region\"
   This is a somewhat limited variation, since you can't specify any additional query options. But
   it's useful if you want overall counts, e.g. 'how many U.S. restaurants are there in each locality?':

     (facets :us-restaurants \"locality\")"
  facets-disp)

(defmethod facets :q
  [q]
  (execute-request (facets* q)))

(defmethod facets :table-and-q
  [table q]
  {:pre [(:select q)]}
  (facets (assoc q :table table)))

(defmethod facets :table-and-select
  [table select]
  (facets {:table table :select select}))

;;;

(defn schema*
  "Returns a query for schema requests, which can be passed into 'execute-request' or used within 'multi'."
  [table]
  {:path (str "t/" (name table) "/schema")})

(defn schema
  "Returns the schema of the specified table, as a hash-map. Example usage:
   (schema :places)"
  [table]
  (execute-request (schema* table)))

;;;

(defn resolve*
  "Returns a query for resolve requests, which can be passed into 'execute-request' or used within 'multi'."
  [values]
  {:path "places/resolve" :params {:values values}})

(defn resolve
  "Takes a hash-map of values indicating what you know about a place. Returns a result
   set with exactly one record as a hash-map if the Factual platform found a suitable
   candidate that meets the criteria you specified. Returns an empty result set otherwise."
  [values]
  (execute-request (resolve* values)))

(defn resolved
  "DEPRECATED. Use resolve, which now returns either one entity or none. One if
   Resolve found a confident match, none if not."
  {:deprecated "1.3.2"}
  [values]
  (->> values
    resolve
    (filter :resolved)
    first))

;;;

(defn match*
  "Returns a query for match requests.  Can be passed into 'execute-request', or used within 'multi'."
  [values]
  {:path "places/match" :params {:values values}})

(defn match
  "Attempts to match values. When a match is found, returns a result set with exactly one hash-map,
   which holds :factual_id. When the Factual platform cannot identify your entity unequivocally,
   returns an empty results set."
  [values]
  (execute-request (match* values)))

;;;

(defn- transform-diffs-response [^InputStream input-stream]
  {:close #(.close input-stream)
   :stream (->> input-stream
             io/reader
             line-seq
             (remove empty?)
             (map #(json/parse-string % true)))})

(defn diffs-query
  "Returns a query for diff requests, which can be passed into 'execute-request'."
  ([table values]
     (diffs-query (assoc values :table table)))
  ([values]
     {:pre [(:table values) (:start values) (:end values)]}
     
     {:path (str "t/" (:table values) "/diffs")
      :params (dissoc values :table)
      :raw-request {:as :stream}
      :response-transform #(->> % transform-diffs-response :stream (map stringify-keys))}))

(defn diffs
  "diffs is used to view changes to a table.

   There are two variations.
   Variation 1: [values]
   Values is a map containing a value for :table, :start, and :end.
   The start and end dates are epoch timestamps in milliseconds.

   Variation 2: [table values]
   The two arguments are the name of the table to obtain diffs for and a map
   containing a :start and :end, which are epoch timestamps in ms.

   Ex. (diffs {:table \"places-us\", :start 1318890505254 :end 1318890516892})
       (diffs \"places-us\" {:start 1318890505254 :end 1318890516892})

   Returns a sequence of zero or more changes."
  ([values]
     (execute-request (diffs-query values)))
  ([table values]
     (execute-request (diffs-query table values))))

;;;

(defn multi-query
  "Returns a query for multi requests, which be passed into 'execute-request'."
  [map]
  {:pre [(:api map) (:args map)]}
  
  (let [f (:api map)
        req-map (apply f (:args map))
        url-params (generate-query-string  (json-params (:params req-map)))]
    (str "/" (:path req-map) (when-not (empty? url-params) "?") url-params)))

(defn multi
  "'m' is a hash-map specifying the full queries. The keys are the names of the queries, and the values are
   hash-maps containing the api and args.

   Required entry within the value hash-map:
     :api   Any one of the apis in the driver with an asterisk suffix. These will prepare a request instead
            of sending off the request. Examples include fetch*, schema*, etc.
     :args  An array of the parameters normally passed to your specific api call

   Example usage:

     (multi {:query1 {:api fetch* :args [{:table :global :q \"cafe\" :limit 10}]}
             :query2 {:api facets* :args [{:table :global :select \"locality,region\" :q \"http://www.starbucks.com\"}]}
             :query3 {:api reverse-geocode* :args [34.06021 -118.41828]}})"
  [m]
  (let [queries  (into {} (for [[k v] m] [k (multi-query v)]))]
    (execute-request {:method :get :path "multi" :params {:queries (json/generate-string queries)} })))

;;;

(defn submit*
  "Returns a query for submit requests, which can be passed into 'execute-request' or used within 'multi'."
  ([s]
     (submit* nil s))
  ([id s]
     {:pre [(:table s) (:values s) (:user s)]}
     
     (let [path (if id
                  (str "t/" (name (:table s)) "/" (name id) "/submit")
                  (str "t/" (name (:table s)) "/submit"))
           params {:user (:user s)}]
       {:path path :method :post :params params :content {:values (:values s)}})))

(defn submit
  ;; TODO: doc-string here
  ([id s] (execute-request (submit* id s)))
  ([s] (execute-request (submit* s))))

;;;

(defn flag*
  "Returns a query for flag requests, which can be passed into 'execute-request' or used within 'multi'."
  [id f]
  {:pre [(:table f) (:problem f) (:user f)]}
  
  (let [path (str "t/" (name (:table f)) "/" (name id) "/flag")
        content (select-keys f [:problem :user :comment :reference])]
    {:path path :method :post :content content}))

(defn flag
  "Flags a specified entity as problematic.

   id is the Factual ID for the entity to flag.

   f must be a hash-map containing:
     :table :problem :user
   f may optionally contain
     :comment :reference

   :problem must be one of:
     :duplicate, :inaccurate, :inappropriate, :nonexistent, :spam, :other"
  [id f]
  (execute-request (flag* id f)))

;;;

(defn geopulse*
  "Returns a query for geopulse requests, which can be passed into 'execute-request' or used within 'multi'."
  [q]
  {:path "places/geopulse" :params q})

(defn geopulse
  "Runs a Geopulse request against Factual and returns the results.

   q is a hash-map specifying the Geopulse query. It must contain :geo.
   It can optionally contain :select. If :select is included, it must be a
   comma delimited list of available Factual pulses, such as \"income\",
   \"race\", \"age_by_gender\", etc.

   Example usage:
   (geopulse {:geo {:$point [34.06021,-118.41828]}})

   Example usage:
   (geopulse {:geo {:$point [34.06021,-118.41828]} :select \"income,race,age_by_gender\"})"
  [q]
  (execute-request (geopulse* q)))

;;;

(defn reverse-geocode*
  "Returns a query for reverse-geocode requests, which can be passed into 'execute-request' or used within 'multi'."
  [lat lon]
  {:path "places/geocode" :params {:geo { :$point [lat lon]}}})

(defn reverse-geocode
  "Given latitude lat and longitude lon, uses Factual's reverse geocoder to return the
   nearest valid address.

   Example usage:

     (reverse-geocode 34.06021,-118.41828)"
  [lat lon]
  (execute-request (reverse-geocode* lat lon)))

;;;

(defn monetize*
  "Returns a query for monetize requests, which can be passed into 'execute-request' or used within 'multi'."
  [params]
  {:path "places/monetize" :params params})

(defn monetize
  "Runs a Monetize request against Factual and returns the results.

   Params should be a hash-map holding your query parameters, such as:
     :q for full text search,
     :fitlers for row filters,
     :geo for a geo filter,
     etc.

   Example usage:

     (monetize {:q \"Fried Chicken, Los Angeles\"})"
  [params]
  (execute-request (monetize* params)))
