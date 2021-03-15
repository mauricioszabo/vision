(ns vision.web
  (:require [sci.core :as sci]
            [sci.impl.namespaces :as sci-ns]
            [clojure.set :as set]
            [reagent.core :as r]
            [reagent.dom :as r-dom]))

(defn- error-boundary [_ #_comp]
  (let [error (r/atom nil)
        info (r/atom nil)]
    (r/create-class
     {:component-did-catch (fn [_ _ i]
                             (reset! info i))
      :get-derived-state-from-error (fn [e]
                                      (reset! error e)
                                      #js {})
      :reagent-render (fn [comp]
                        (if @error
                          [:div "Something went wrong."]
                          comp))})))

(defn- render-function
  [{:keys [fun data]}]
  (let [id (str (gensym "html-component-"))]
    (r/create-class
     {:display-name "render-function"
      :reagent-render (fn [] [:div {:id id}])
      :component-did-mount (fn [this]
                             (fun (r-dom/dom-node this) data))

      :component-will-update (fn [this [_ {:keys [f data]}]]
                               ; (info (str "jsrender new params: " data))
                               (fun (r-dom/dom-node this) data))})))

(defn- render-clj [data]
  [error-boundary [render-function data]])

(defn- render-js
  "reagent component that renders a js function,
       calls
       parameters:
         fun  the js render function
              gets js data
         data a clojure datastructure that will be converted to js
              before calling f"
  [params]
  (let [data-js (update params :data clj->js)]
    [render-clj data-js]))

(defn to-reagent [fun]
  (fn [ & args]
    [render-js
     {:fun (fn [dom args]
             (let [div (.createElement js/document "div")
                   upd (fn [elem]
                         (try (.removeChild dom div) (catch :default _))
                         (.appendChild dom elem))
                   elem (apply fun (js->clj args))]
               (.. div -classList (add "loading"))
               (.appendChild dom div)
               (if (instance? js/Promise elem)
                 (.then elem upd)
                 (upd elem))))
      :data args}]))

(def ^:private dom-funs
  {'create (fn [tag] (js/document.createElement tag))
   'add-class! (fn [elem classes]
                 (let [classlist (.-classList elem)]
                   (doseq [class classes]
                     (.add classlist class))))
   'set-text! (fn [elem text] (aset elem "innerText" text))})

(def ^:private js-funs
  {'eval #(js/eval %)})

(def ^:private json-funs
  {'to-string #(-> % clj->js js/JSON.stringify)})

(def ^:private namespaces
  (-> sci-ns/namespaces
      (set/rename-keys '{clojure.string str
                         clojure.set set
                         clojure.walk walk
                         clojure.template template
                         clojure.repl repl
                         clojure.edn edn})
      (assoc 'dom dom-funs)
      (assoc 'js js-funs)
      (assoc 'json json-funs)))

(defn evaluate! [code]
  (sci/eval-string code
                   {:classes {:allow :all}
                    :namespaces namespaces}))

(defn render [selector code]
  (let [result (cond-> code (string? code) evaluate!)
        selector (js/document.querySelector selector)]
    (r-dom/render result selector)))
