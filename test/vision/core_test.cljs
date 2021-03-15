(ns vision.core-test
  (:require [clojure.test :refer [deftest]]
            [vision.web :as web]
            [promesa.core :as p]
            [check.async :refer [check async-test]]
            [shadow.resource :as resource]))

(def edn-vega
  {:$schema "https://vega.github.io/schema/vega-lite/v4.json"
   :width "container"
   :height 250
   :data {:values [{"Origin" "USA"}
                   {"Origin" "USA"}
                   {"Origin" "Japan"}]}
   :mark "bar"
   :encoding {:x {:field "Origin"}
              :y {:aggregate "count"
                  :title "Number of Cars"}}})

(defn await-for [fun]
  (p/loop [n 0]
    (p/let [res (fun)]
      (cond
        res res
        (< n 50) (p/do! (p/delay 100) (p/recur (inc n)))
        :else nil))))

(defn- prepare-fixture-div! []
  (p/let [root (await-for #(js/document.querySelector "#test-root"))
          div (or (js/document.querySelector "div#test-fixture")
                  (js/document.createElement "div"))
          div (doto div
                    (aset "id" "test-fixture")
                    (.. -classList (add "container")))]
    (.appendChild root div)
    div))

(defn- await-for-text [text]
  (await-for #(let [doc (js/document.querySelector "#test-fixture")
                    inner (some-> doc .-innerText)]
                (and (= inner text) text))))

(deftest rendering-hiccups
  (async-test "Renders a hiccup tag" {:timeout 6000}
    (prepare-fixture-div!)
    (let [code (pr-str '(let [code "Hello, world"]
                          [:div code]))]
      (web/render "#test-fixture" code)
      (check (await-for-text "Hello, world") => "Hello, world"))))

(deftest rendering-cljs-functions
  (async-test "Renders based on a ClojureScript function" {:timeout 6000}
    (prepare-fixture-div!)
    (let [code (pr-str '(fn [person]
                          (let [div (dom/create "div")]
                            (dom/add-class! div ["container"])
                            (dom/set-text! div (str "Hello, " person))
                            div)))
          component (-> code web/evaluate! web/to-reagent)]
      (web/render "#test-fixture" [component "Ariovaldo"])
      (check (await-for-text "Hello, Ariovaldo") => "Hello, Ariovaldo"))))

(def vega-code
  (resource/inline "vega-bundled.cljs"))

(deftest rendering-vega-bundled-visualizer
  (async-test "Renders a hiccup tag" {:timeout 6000}
    (prepare-fixture-div!)
    (let [component (-> vega-code (str "\nvega-tag") web/evaluate! web/to-reagent)]
      (web/render "#test-fixture" [component edn-vega])
      (check (await-for #(js/document.querySelector "#test-fixture div.chart-wrapper"))
             => some?))))
