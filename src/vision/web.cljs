(ns vision.web
  (:require [sci.core :as sci]
            [sci.impl.namespaces :as sci-ns]
            [clojure.set :as set]
            [reagent.dom :as r-dom]))

(def ^:private namespaces
  (-> sci-ns/namespaces
      (set/rename-keys '{clojure.string str
                         clojure.set set
                         clojure.walk walk
                         clojure.template template
                         clojure.repl repl
                         clojure.edn edn})))
(defn evaluate! [code]
  (sci/eval-string code
                   {:classes {:allow :all}
                    :namespaces namespaces}))

(defn render [selector code]
  (let [result (evaluate! code)
        selector (js/document.querySelector selector)]
    (r-dom/render result selector)))
; (def foo 10)
; (def exports #js {:foo foo})
