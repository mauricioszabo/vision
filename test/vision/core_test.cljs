(ns vision.core-test
  (:require [clojure.test :refer [deftest is]]
            [vision.web :as web]
            ; [vision.node :as node]
            [promesa.core :as p]
            [check.async :refer [check testing async-test]]
            [shadow.resource :as resource]))

(def edn-vega
  {:$schema "https://vega.github.io/schema/vega-lite/v4.json",
   :width "container"
   :height 250,
   :data {:values [{"Origin" "USA"}
                   {"Origin" "USA"}
                   {"Origin" "Japan"}]
          :mark "bar"
          :encoding {:x {:field "Origin"},
                     :y {:aggregate "count",
                         :title "Number of Cars"}}}})

(def vega-code
  (resource/inline "vega_test.cljs"))

(defn await-for [fun]
  (p/loop [n 0]
    (p/let [res (fun)]
      (cond
        res res
        (< n 50) (p/do! (p/delay 100) (p/recur (inc n)))
        :else nil))))

(defn- prepare-fixture-div! []
  (p/let [root (await-for #(js/document.querySelector "#test-root"))
          div (doto (js/document.createElement "div")
                    (aset "id" "test-fixture")
                    (.. -classList (add "container")))]
    (.appendChild root div)
    div))

; (def simple-code
;   '(let [root (dom/query "#test-root")
;          container (doto (dom/create "div")
;                          (dom/set-text "Hello, world"))]
;      [:div ]))

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

(def simple-code
  (str '(def some-text (js/require "some-lib"))
       "\n\n"
       '[:div some-text]))

#_
(deftest bunding-simple-packages
  (async-test "Bundles a code and renders the hiccup" {:timeout 6000}
    (prepare-fixture-div!)
    (p/let [bundled (node/bundle-cljs simple-code
                                      {:input-fs {:stat (fn [a callback]
                                                          (prn :STAT a)
                                                          (cond
                                                            (re-find #"node_modules" a)
                                                            (callback nil
                                                             #js {:isFile (constantly false)
                                                                  :isDirectory (constantly true)})

                                                            (re-find #"some-lib\.js" a)
                                                            (callback nil
                                                                      #js {:isFile (constantly true)
                                                                           :isDirectory (constantly false)})))
                                                  ;:readlink
                                                  :readdir (fn [_ callback]
                                                             (prn :READDIR)
                                                             (callback #js ["some-lib.js"]))
                                                  :readFile (fn [_ callback]
                                                              (prn :READFILE _)
                                                              (callback "Hello, Bundled"))}})]
      (prn :B bundled))))
      ;     code (pr-str '(let [code "Hello, world"]
      ;                     [:div code]))]
      ; (web/render "#test-fixture" code)
      ; (check (await-for-text "Hello, world") => "Hello, world"))))


#_
(deftest bundler-for-js-objects
  (async-test "Lol, wow"
    (let [fixture (prepare-fixture-div!)]
      (p/do!
       (p/delay 1000)
       (check (p/delay 200 {:foo 1}) => {:foo 20})))))
