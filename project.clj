(defproject factual/factual-clojure-driver "1.4.7"
  :url "http://github.com/Factual/factual-clojure-driver"
  :description "Officially supported Clojure driver for Factual's public API"
  :dependencies [
    [org.clojure/clojure "1.4.0"]
    [org.clojure/data.json "0.2.1"]
    [slingshot "0.10.2"]
    [oauth-clj "0.1.2"
     :exclusions [org.slf4j/slf4j-log4j12]]
    [clj-http "0.5.2"]] ;oauth-clj has dependency on clj-http as well, but the debug feature for
                        ;that version didn't work
  :dev-dependencies [[factual/sosueme "0.0.14"]
                     [lein-clojars "0.6.0"]
                     [oauth-clj "0.1.2"
                      :exclusions [org.slf4j/slf4j-log4j12]]])
