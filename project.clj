(defproject ledjure "0.1"
  :description "Toy cryptocurrency implemented in Clojure"
  :url "http://github.com/danielstockton/clojure-coin"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[aleph "0.4.3"]
                 [base58 "0.1.0"]
                 [com.cognitect/transit-clj "0.8.300"]
                 [gloss "0.2.5"]
                 [integrant "0.6.1"]
                 [io.aviso/pretty "0.1.34"]
                 [org.clojure/clojure "1.9.0-alpha10"]
                 [org.clojure/spec.alpha "0.1.123"]
                 [overtone/at-at "1.2.0"]]
  :profiles {:dev {:dependencies [[integrant/repl "0.2.0"]]
                   :source-paths ["dev"]}}
  :main ledjure.core)
