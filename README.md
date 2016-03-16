# About

This is the Factual-supported Clojure driver for [Factual's public API](http://developer.factual.com).

# Installation

The driver is hosted at [Clojars](https://clojars.org/factual/factual-clojure-driver). Just add this to your dependencies:

```clojure
[factual/factual-clojure-driver "1.5.4"]
```

# Setup

```clojure
(ns yournamespace.core
  (:require [factual.api :as fact]))
  (fact/factual! "YOUR_FACTUAL_KEY" "YOUR_FACTUAL_SECRET")
```

If you don't have a Factual API account yet, [it's free and easy to get one](https://www.factual.com/api-keys/request).

# Fetch

The `fetch` function supports rich read queries. It takes a hash-map as its argument, which specifies the full query. The only required entry is :table, which must be associated with a valid Factual table name. Optional query parameters, such as row filters and geolocation, are specified with further entries in q.

`fetch` returns a sequence of records, where each record is a hash-map representing a row of results. The returned sequence will have response metatada attached to it, which includes things like API version, status, and extra row count information if it was requested.

Simple example:

```clojure
;; Fetch 3 random Places from Factual
(fact/fetch {:table :places :limit 3})
```
Results might look like:

```clojure
[{:status 1, :country US, :longitude -94.819339, :name Lorillard Tobacco Co., :postcode 66218, ... }
 {:status 1, :country US, :longitude -118.300024, :name Imediahouse, :postcode 90005, ... }
 {:status 1, :country US, :longitude -118.03132, :name El Monte Wholesale Meats, :postcode 91733, ... }]
```

Here's a demo of looking at the metadata of a response:

```clojure
> (def res (fact/fetch {:table :places}))
> (meta res)
{:response {:included_rows 3}, :version 3, :status "ok"}
```

## More Examples

```clojure
;; Sample three business names from the Places dataset (U.S. points of interest):
> (map :name (fact/fetch {:table :places :limit 3}))
("Lorillard Tobacco Co." "Imediahouse" "El Monte Wholesale Meats")
```

```clojure
;; Return rows where region equals "CA"
(fact/fetch {:table :places :filters {"region" "CA"}})
```

```clojure
;; Return rows where name begins with "Starbucks". Return both the data and a total count of the matched rows:
(fact/fetch {:table :places :filters {:name {:$bw "Starbucks"}} :include_count true})
```

```clojure
;; Do a full text search for rows that contain "Starbucks" or "Santa Monica"
(fact/fetch {:table :places :q "Starbucks,Santa Monica"})
```

```clojure
;; Do a full text search for rows that contain "Starbucks" or "Santa Monica" and return rows 20-40
(fact/fetch {:table :places :q "Starbucks,Santa Monica" :offset 20 :limit 20})
```

```clojure
;; Return rows with a name equal to "Stand" within 5000 meters of the specified lat/lng
(fact/fetch {:table :places
         :filters {:name "Stand"}
         :geo {:$circle {:$center [34.06018, -118.41835] :$meters 5000}}})
```

```clojure
;; Count all businesses in Chiang Mai, Thailand that are operational and have a telephone number
(get-in
  (meta
      (fact/fetch {:table :global
                       :include_count true
                   :filters {:country {:$eq "TH"}
                         :region {:$eq "Chiang Mai"}
                                 :status {:$eq 1}
                                 :tel {:$blank false}}}))
      [:response :total_row_count])
```

```clojure
;; Define function that finds restaurants near a given latitude/longitude that deliver dinner, sorted by distance:
(defn deliver-dinner [lat lon]
  (fact/fetch {:table :restaurants
           :filters {:meal_dinner {:$eq true}
                     :meal_deliver {:$eq true}}
           :geo {:$circle {:$center [lat lon]
                           :$meters 4500}}
           :sort :$distance}))
```

You could use the above function like so:

```clojure
(deliver-dinner 34.039792 -118.423421)
```

## Variations of `fetch`

For added convenience, the `fetch` function supports several other argument variations. For example, this will work:

```clojure
(fact/fetch :places {:limit 3})
```

See the [docs on `fetch`](https://github.com/Factual/factual-clojure-driver/wiki/Fetch) for more details.

## Using Fetch with any Factual dataset

`fetch` allows you to specify any valid Factual dataset. E.g.:

```clojure
(fact/fetch {:table :global :limit 12})
(fact/fetch {:table :places :q "starbucks"})
(fact/fetch {:table :restaurants :filters {:locality "Los Angeles"}})
(fact/fetch {:table :products-cpg :filters {:brand "The Body Shop"}})
```

# Fetch A Row

Runs a 'Get A Row' request against Factual for the specified table and row and returns exactly one result as a hash-map in a result set (if the row exists).
Example usage:

```clojure
(fact/fetch-row :places "03d401b7-e4f3-4216-b1c9-5bb08be3d786")
```

If the specified row does not exist, an error will be thrown.

# Row Filters

The driver supports all available row filtering logic. Examples:

```clojure
;;; Fetch places whose name field starts with "Starbucks"
(fact/fetch {:table :places :filters {:name {:$bw "Starbucks"}}})
```

```clojure
;;; Fetch U.S. restaurants that have a blank telephone number
(fact/fetch {:table :restaurants :filters {:tel {:$blank true}}})
```

```clojure
;;; Fetch U.S. restaurants from one of five states
(fact/fetch {:table :restaurants
             :filters {:region {:$in ["MA", "VT", "NH", "RI", "CT"]}}})
```

## Supported row filter logic

<table>
  <tr>
    <th>Predicate</th>
    <th>Description</th>
    <th>Example :filters</th>
  </tr>
  <tr>
    <td>$eq</td>
    <td>equal to</td>
    <td>{:region {:$eq "CA"}}</td>
  </tr>
  <tr>
    <td>$neq</td>
    <td>not equal to</td>
    <td>{:region {:$neq "CA"}}</td>
  </tr>
  <tr>
    <td>$search</td>
    <td>full text search</td>
    <td>{:name {:$search "fried chicken"}}</td>
  </tr>
  <tr>
    <td>$in</td>
    <td>equals any of</td>
    <td>{:region {:$in ["MA", "VT", "NH", "RI", "CT"]}}</td>
  </tr>
  <tr>
    <td>$nin</td>
    <td>does not equal any of</td>
    <td>{:region {:$nin ["MA", "VT", "NH", "RI", "CT"]}}</td>
  </tr>
  <tr>
    <td>$bw</td>
    <td>begins with</td>
    <td>{:name {:$bw "starbucks"}}</td>
  </tr>
  <tr>
    <td>$nbw</td>
    <td>does not begin with</td>
    <td>{:name {:$nbw "starbucks"}}</td>
  </tr>
  <tr>
    <td>$bwin</td>
    <td>begins with any of</td>
    <td>{:name {:$bwin ["starbucks" "tea" "coffee"]}}</td>
  </tr>
  <tr>
    <td>$nbwin</td>
    <td>does not begin with any of</td>
    <td>{:name {:$nbwin ["starbucks" "tea" "coffee"]}}</td>
  </tr>
  <tr>
    <td>$blank</td>
    <td>whether is blank or null</td>
    <td>{:name {:$blank true}}<br>
        {:name {:$blank false}}</td>
  </tr>
  <tr>
    <td>$gt</td>
    <td>greater than</td>
    <td>{:rating {:$gt 3.0}}</td>
  </tr>
  <tr>
    <td>$gte</td>
    <td>greater than or equal to</td>
    <td>{:rating {:$gte 3.0}}</td>
  </tr>
  <tr>
    <td>$lt</td>
    <td>less than</td>
    <td>{:rating {:$lt 3.0}}</td>
  </tr>
  <tr>
    <td>$lte</td>
    <td>less than or equal to</td>
    <td>{:rating {:$lte 3.0}}</td>
  </tr>
  <tr>
    <td>$includes</td>
    <td>array includes</td>
    <td>{:category_ids {:$includes 308}}</td>
  </tr>
  <tr>
    <td>$includes_any</td>
    <td>array includes any of</td>
    <td>{:category_ids {:$includes_any [312 338]}}</td>
  </tr>
</table>

# World Geographies

World Geographies contains administrative geographies (states, counties, countries), natural geographies (rivers, oceans, continents), and assorted geographic miscallaney.  This resource is intended to complement Factual's Global Places and add utility to any geo-related content.

Common use cases include:

* Determining all cities within a state or all postal codes in a city
* Creating a type-ahead placename lookup
* Validating data against city, state, country and county names
* A translation table to convert between the search key used by a user, i.e. '慕尼黑' or 'Munich' for the native 'München'

You can use the `fetch` function to query World Geographies, supplying :world-geographies as the table name.

Examples:

```clojure
; Get all towns surrounding Philadelphia
(fact/fetch {:table :world-geographies
             :select "neighbors"
             :filters {:factual_id {:$eq "08ca0f62-8f76-11e1-848f-cfd5bf3ef515"}}})
```

```clojure
; Find the town zipcode 95008 belongs to
(fact/fetch  {:table :world-geographies
              :filters {:name {:$eq "95008"}
                        :country {:$eq "us"}}})
```

```clojure
; Searching by placename, placetype, country and geographic hierarchy
(fact/fetch {:table :world-geographies
             :filters {:name {:$eq "wayne"}
                       :country {:$eq :us}
                       :placetype {:$eq "locality"}
                       :ancestors {:$search "08666f5c-8f76-11e1-848f-cfd5bf3ef515"}}})
```

For more details about World Geographies, including schema, see [the main API docs for World Geographies](http://developer.factual.com/display/docs/World+Geographies).

# Facets

The `facets` function gives you row counts for Factual tables, grouped by facets of the data. For example, you may want to query all businesses within 1 mile of a location and for a count of those businesses by category:

```clojure
(fact/facets {:table :restaurants
              :select "category"
              :geo {:$circle {:$center [34.039792 -118.423421]
                              :$meters 1600}}})
```

The argument to facets is a hash-map of query parameters, and must include entries for `:table` and `:select`. The value for `:select` must be a comma-delimited String indicating which field(s) to facet, e.g. `"locality,region"`.

Not all fields are configured to return facet counts.  To determine what fields you can return facets for, use the `schema` function on the relevant table.  The faceted attribute of the returned schema will let you know.

## Variations of `facets`

For added convenience, the `facets` function supports several other argument variations. For example, this will work:

```clojure
(fact/facets :us-restaurants "locality")
```

See the docs on `facets` for more details.

## More `facets` Examples

```clojure
;; Count Starbucks in the US by city and state
(fact/facets {:table :global :select "locality,region" :q "starbucks" :filters {:country :US}})
```

# Crosswalk

Crosswalk provides a translation between Factual IDs, third party IDs, and URLs that represent the same entity across the internet. You use Crosswalk as a table called 'crosswalk-[ISO two letter country code]'.

Examples:

```clojure
;; Lookup the Yelp Crosswalk entry for The Stand, using on its Yelp page
(fact/fetch {:table :crosswalk-us
             :filters {:url "http://www.yelp.com/biz/the-stand-los-angeles-5"}})
```

```clojure
;; Lookup The Stand's Crosswalk entry using its Foursquare ID
(fact/fetch {:table :crosswalk-us
             :filters {:namespace :foursquare
                       :namespace_id "4a651cb1f964a52052c71fe3"}})
```

```clojure
;; Find all Crosswalk entries that Factual has for The Stand
(fact/fetch {:table :crosswalk-us
             :filters {:factual_id "3b9e2b46-4961-4a31-b90a-b5e0aed2a45e"}})
```

```clojure
;; Find the OpenMenu Crosswalk entry for The Stand, by Factual ID
(fact/fetch {:table :crosswalk
             :filters {:factual_id "3b9e2b46-4961-4a31-b90a-b5e0aed2a45e"
                       :namespace :openmenu}})
```

```clojure
;; Search for all Yelp Crosswalk entries for The Container Store
(fact/fetch {:table :crosswalk
             :q "the container store"
             :filters {:namespace :yelp}})
```

More details on Crosswalk can be found in [our general API documentation for Crosswalk](http://developer.factual.com/display/docs/Places+API+-+Crosswalk).

# Resolve

Use the `resolve` function to enrich your data and match it against Factual's.

The `resolve` function takes a hash-map of values indicating what you know about a place. Returns a result set with exactly one record as a hash-map if the Factual platform found a suitable candidate that meets the criteria you specified. Returns an empty result set otherwise.

Example:

```clojure
; Find the entity named "McDonald's" with only a specified lat/lng
(fact/resolve {:name "McDonalds", :latitude 34.05671 :longitude -118.42586})
```

# Match

The `match` function attempts to find the Factual ID of the data that matches your data. When a match is found, it returns a result set with exactly one hash-map, which holds :factual_id. When the Factual platform cannot identify your entity unequivocally, the `match` function returns an empty results set.

Examples:

```clojure
; Find the entity named "McDonald's" with an address combination
(fact/match {:name "McDonalds"
             :address "10451 Santa Monica Blvd"
             :region "CA"
             :postcode "90025"})
```

```clojure
; Find the entity named "McDonald's" with only a specified lat/lng
(fact/match {:name "McDonalds" :latitude 34.05671 :longitude -118.42586})
```

# Results Metadata

Factual's API returns more than just results rows. It also returns various metadata about the results. You can access this metadata by using Clojure's `meta` function on your results. Examples:

```clojure
> (meta (fact/fetch {:table :places
                     :filters {:name {:$bw "Starbucks"}}
                     :include_count true}))
{:total_row_count 8751, :included_rows 20, :version 3, :status "ok"}
```

```clojure
> (meta (fact/crosswalk :factual_id "97598010-433f-4946-8fd5-4a6dd1639d77"))
{:total_row_count 13, :included_rows 13, :version 3, :status "ok"}
```

# Schema

You can get the schema for a specific table like this:

```clojure
(fact/schema :restaurants)
```

# Submit

_NOTICE_: At the current time, this API call is ONLY compatible with places-v3. Please see the [the migration page](http://developer.factual.com/display/docs/Places+API+-+v3+Migration) for more details.

The `submit` function lets you submit new or corrected data to Factual. Examples:

```clojure
; Submit a new entity to Factual's U.S. Restaurants table
(fact/submit {:table :places :user "boris123" :values {:name "A New Restaurant" :locality "Los Angeles"}})
```

```clojure
; Submit a correction to an existing entity in Factual's U.S. Restaurants table
(fact/submit {:table :places :user "boris123" :values {:factual_id "97598010-433f-4946-8fd5-4a6dd1639d77" :name "New Name"}})
```

The :user parameter is required, and specifies the identity of the end user that is submitting the data. This may be you, or it may be one of your users.

# Flag

_NOTICE_: At the current time, this API call is ONLY compatible with places-v3. Please see the [the migration page](http://developer.factual.com/display/docs/Places+API+-+v3+Migration) for more details.

NOTICE: Server support for this feature is still under development. You are getting a preview of how this driver will support the feature. If you try using this feature now, you may not get a successful response. We will remove this notice once the feature is fully supported.
The `flag` function lets you flag a Factual entity as problematic. For example:

```clojure
(fact/flag "97598010-433f-4946-8fd5-4a6dd1639d77" {:table :places :problem :spam :user "boris_123"})
```
The first argument is the Factual ID of the entity you wish to flag.

The second argument is a hash-map that specifies the flag, f.

The :problem entry in f is required, and must be one of:

<ul>
<li>:duplicate
<li>:inaccurate
<li>:inappropriate
<li>:nonexistent
<li>:spam
<li>:other
</ul>

The :user in f is required, and specifies the identity of the end user that is submitting the data. This may be you, or it may be one of your users.

f may optionally contain entries for:
<ul>
<li>:comment
<li>:reference
</ul>

# Geopulse

Factual Geopulse provides point-based access to geographic attributes: you provide a long/lat coordinate pair, Factual provides everything it knows about that geography.

The Geopulse API is made up of several "pulses".  Pulses are georeferenced attributes generated by Factual, sourced from openly available content (such as the US Census), or provided to Factual by proprietary third-parties.

You can run a Geopulse query using the `geopulse` function. You pass it a hash-map specifying the query parameters. It must contain `:geo`. It can optionally contain `:select`. If `:select` is included, it must be a comma delimited list of available Factual pulses.

 Example usage:

 ```clojure
(fact/geopulse {:geo {:$point [34.06021,-118.41828]}})
```

```clojure
(fact/geopulse {:geo {:$point [34.06021,-118.41828]} :select "income,housing"})
```

Available pulses include commercial_density, commercial_profile, income, race, hispanic, and age_by_gender.

You can see a full list of available Factual pulses and their possible return values, as well as full documentation, in [the Factual API docs for Geopuls](http://developer.factual.com/display/docs/Places+API+-+Geopulse).

# Geotag (Fctual's reverse geocoder)

Given a latitude and longitude, uses Factual's reverse geocoder to return the nearest valid address.

Example usage:

```clojure
(fact/reverse-geocode 34.06021,-118.41828)
```

# Monetize

The Monetize API enables you to access offers that Factual has aggregated from various third party offer originators  and earn money based on conversions.  The way it works is Factual snaps offers to Factual Places.  These offers and related places are exposed in the Monetize API, which is accessible through the same API structure as Factual's Core API.

As your users convert (i.e. purchase a deal, order from an online menu), Factual will relay to you a healthy commission from the third party offer originators.  To be clear, such payment is based on the actual conversions driven by a given developer.

Examples:

```clojure
;; Full-text search
(fact/monetize {:q \"Fried Chicken, Los Angeles\"})
```

```clojure
;; Row Filter on a given city (place locality)
(fact/monetize {:filters {:place_locality :Philadelphia}})
```

```clojure
;; Geo Filter
(fact/monetize {:geo {:$circle {:$center [34.06018,-118.41835]
                                :$meters 5000}}})
```

```clojure
;; Geo Filter Limited to Groupon deals
(fact/monetize {:geo {:$circle {:$center [34.06018,-118.41835]
                                :$meters 5000}}
                :filters {:source_namespace {:$eq :groupon}}})
```

```clojure
;; Row Filter on a given city and Yelp
(fact/monetize {:filters {:$and [{:place_locality {:$eq :Boston}}
                                 {:source_namespace {:$eq :yelp}}]}})
```

```clojure
;; Row Filter on a given city and exclude Yelp
(fact/monetize {:filters {:$and [{:place_locality {:$eq :Boston}}
                                 {:source_namespace {:$neq :yelp}}]}})
```

For more details on Monetize, including schema, see [the main API docs](http://developer.factual.com/display/docs/Places+API+-+Monetize)

# Diffs

`diffs` is most useful for users who download Factual's dataset and use it locally.  It returns a sequence containing all changes to the given dataset between `:start` and `:end`.  The beginning and end times are represented as epoch timestamps in milliseconds.

Example:

```clojure
 (fact/diffs {:table "global" :start 1318890505254 :end 1318890516892})
 (fact/diffs "global" {:start 1318890505254 :end 1318890516892})
```

For more details on Diffs, see [the main API docs](http://developer.factual.com/display/docs/Core+API+-+Diffs)

# Multi

Multi provides a means to issue multiple api calls with one http request. The argument is a hash-map specifying the full queries. The keys are the names of the queries, and the values are hash-maps containing the api and args.

Required entry within the value hash-map:
<ul>
<li>:api  Any one of the apis in the driver with an asterisk suffix. These will prepare a request instead of sending off the request. Examples include fetch*, schema*, etc.
<li>:args An array of the parameters normally passed to your specific api call
</ul>

Example:

```clojure
 (fact/multi {:query1 {:api fact/fetch* :args [{:table :global :q "cafe" :limit 10}]}
              :query2 {:api fact/facets* :args [{:table :global :select "locality,region" :q "http://www.starbucks.com"}]}
              :query3 {:api fact/reverse-geocode* :args [34.06021 -118.41828]}})

```

For more details on Multi, see [the main API docs](http://developer.factual.com/display/docs/Core+API+-+Multi)

# Handling Bad Responses

The driver uses Slingshot to indicate API errors. If an API error is encountered, a Slingshot stone called factual-error will be thrown.

The factual-error will contain information about the error, including the server response code and any options you used to create the query.

Example:

```clojure
;  (:import [factual.api factual-error])
(try+
    (fact/fetch {:table :places :filters {:factual_id "97598010-433f-4946-8fd5-4a6dd1639d77" :BAD :PARAM}})
    (catch factual-error {code :code message :message opts :opts}
      (println "Got bad resp code:" code)
      (println "Message:" message)
      (println "Opts:" opts)))
```

# Example Use Case

Let's create a function that finds Places close to a lat/lng, with "cafe" in their name:

```clojure
(defn nearby-cafes
    "Returns up to 12 cafes within 5000 meters of the specified location."
    [lat lon]
    (fact/fetch {:table :places
                      :q "cafe"
                  :filters {:category {:$eq "Food & Beverage"}}
                  :geo {:$circle {:$center [lat lon]
                                  :$meters 5000}}
                  :include_count true
                  :limit 12}))
```

Using our function to get some cafes:

    > (def cafes (nearby-cafes 34.06018 -118.41835))

Let's peek at the metadata:

```clojure
> (meta cafes)
{:total_row_count 26, :included_rows 12, :version 3, :status "ok"}
```

We got back the full limit of 12 results, and we can see there's a total of 26 cafes near us. Let's take a look at a few of the cafes we got back:

```clojure
> (map :name (take 3 cafes))
("Aroma Cafe" "Cafe Connection" "Panini Cafe")
```

That first one, "Aroma Cafe", sounds interesting. Let's see the details:

```clojure
    > (clojure.contrib.pprint/pprint (first cafes))
    {:status "1",
     :country "US",
     :longitude -118.423421,
     :factual_id "eb67e10b-b103-41be-8bb5-e077855b7ae7",
     :name "Aroma Cafe",
     :postcode "90064",
     :locality "Los Angeles",
     :latitude 34.039792,
     :region "CA",
     :address "2530 Overland Ave",
     :website "http://aromacafe-la.com/",
     :tel "(310) 836-2919",
     :category "Food & Beverage"}
```

No let's use Crosswalk to fine out what Yelp has to say about this place. Note that we use Aroma Cafe's :factual_id from the above results...

```clojure
    > (fact/crosswalk :factual_id "eb67e10b-b103-41be-8bb5-e077855b7ae7" :only "yelp")
    ({:factual_id "eb67e10b-b103-41be-8bb5-e077855b7ae7",
      :namespace :yelp,
      :namespace_id "AmtMwS2wCbr3l-_S0d9AoQ",
      :url "http://www.yelp.com/biz/aroma-cafe-los-angeles"})
```

That gives me the yelp URL for the Aroma Cafe, so I can read up on it on Yelp.com.

Of course, Factual supports other Crosswalked sources besides Yelp. If you look at each row returned by the `crosswalk` function, you'll see there's a `:namespace` in each one. Let's find out what namespaces are available for the Aroma Cafe:

```clojure
    > (map :namespace (fact/crosswalk :factual_id "eb67e10b-b103-41be-8bb5-e077855b7ae7"))
    (:merchantcircle :urbanspoon :yahoolocal :foursquare :yelp ... )
```

Let's create a function that takes a :factual_id and returns a hashmap of each valid namespace associated with its Crosswalk URL:

```clojure
(defn namespaces->urls [factid]
    (into {} (map #(do {(:namespace %) (:url %)})
      (fact/crosswalk :factual_id factid))))
```

Now we can do this:

```clojure
    > (namespaces->urls "eb67e10b-b103-41be-8bb5-e077855b7ae7")
    {:merchantcircle   "http://www.merchantcircle.com/business/Aroma.Cafe.310-836-2919",
     :urbanspoon       "http://www.urbanspoon.com/r/5/60984/restaurant/West-Los-Angeles/Bali-Place-LA",
     :yahoolocal       "http://local.yahoo.com/info-20400708-aroma-cafe-los-angeles",
     :foursquare       "https://foursquare.com/venue/46f53d65f964a520f04a1fe3",
     :yelp             "http://www.yelp.com/biz/aroma-cafe-los-angeles",
     ... }
```

# Debug mode

This driver and the Factual service should always work perfectly, all the time. But in the highly unlikely, almost impossible event that things go wrong, there is a debug mode that will help with troubleshooting.

If you wrap your call(s) with the `debug` macro, verbose debug information will be sent to stdout. This will provide details about the request sent to Factual and the response that was returned.

Example use of the `debug` macro:

```clojure
(def data (fact/debug (fact/fetch {:table :places :q "starbucks" :limit 3})))
```

You can also wrap `debug` around the lower-level `get-results` function, like so:

```clojure
(def data (fact/debug (fact/get-results "t/places" {:q "starbucks" :limit 3})))
```

# Where to Get Help

If you think you've identified a specific bug in this driver, please file an issue in the github repo. Please be as specific as you can, including:

  * What you did to surface the bug
  * What you expected to happen
  * What actually happened
  * Detailed stack trace and/or line numbers

If you are having any other kind of issue, such as unexpected data or strange behaviour from Factual's API (or you're just not sure WHAT'S going on), please contact us through [GetSatisfaction](http://support.factual.com/factual).

# License

The use and distribution terms for this software are covered by the
Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
which can be found in the file LICENSE.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be bound by
the terms of this license.
You must not remove this notice, or any other, from this software.
