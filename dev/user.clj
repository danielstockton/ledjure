(ns user
  (:require [integrant.repl :refer [clear go halt init reset reset-all]]
            [ledjure.core :refer [config]]))

(integrant.repl/set-prep! (constantly config))

