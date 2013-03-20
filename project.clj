(defproject factual/factual-clojure-driver "1.4.6"
  :url "http://github.com/Factual/factual-clojure-driver"
  :description "Officially supported Clojure driver for Factual's public API"
  :dependencies [
    [org.clojure/clojure "1.5.1"]
    [factual/sosueme "0.0.14"]                    
    [slingshot "0.10.3"]
    [oauth-clj "0.1.3-SNAPSHOT"
     :exclusions [org.slf4j/slf4j-log4j12]]
    [clj-http "0.6.5"]] ;oauth-clj has dependency on clj-http as well, but the debug feature for
                        ;that version didn't work
  )

;; :dev-dependencies [[factual/sosueme "0.0.14"]
;;                      [lein-clojars "0.6.0"]
;;                      [oauth-clj "0.1.2"
;;                       :exclusions [org.slf4j/slf4j-log4j12]]])
