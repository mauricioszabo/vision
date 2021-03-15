(ns vision.test-helpers)
  ; (:require [clojure.edn :as edn]))

(defn http [req]
  ; (def req req)
  ; (def body (-> req :body slurp edn/read-string))
  ; (def a (-> req :body  .readAllBytes))
  ; (prn (-> req :body  .readAllBytes String.))
  {:status 200
   :body "nok"})
