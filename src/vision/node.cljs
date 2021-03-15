(ns vision.node
  (:require [sci.core :as sci]
            [promesa.core :as p]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [rewrite-clj.node.seq :as seq]
            [rewrite-clj.node.stringz :as node-str]
            [rewrite-clj.node.token :as token]
            [rewrite-clj.node.whitespace :as whitespace]
            [clojure.walk :as walk]
            [shadow.resource :as resource]
            ["webpack" :as webpack]
            ["memfs" :refer [createFsFromVolume Volume]]))

(defn bundle
  "Bundles a bunch of Javascript files with webpack, and return a single
Javascript content. Opts is:

:input-fs - a node.js compatible implementation containing, at least :readFile,
  :stat, :readlink, and :readdir"
  [first-file opts]
  (let [fake-fs (createFsFromVolume (Volume.))
        a (webpack (clj->js {:mode "production"
                             :entry first-file
                             :output {:path "/tmp/dist"
                                      :library {:type "var"
                                                :name "entry"}
                                      :filename "bundle.js"}}))]
    (aset a "outputFileSystem" fake-fs)
    (when-let [input (:input-fs opts)] (aset a "inputFileSystem" (clj->js input)))
    (js/Promise. (fn [resolve fail]
                   (.run a (fn [err succ]
                             (if err
                               (fail err)
                               (try
                                 (-> fake-fs
                                     (.readFileSync "/tmp/dist/bundle.js")
                                     .toString
                                     resolve)
                                 (catch :default e
                                   (fail (or succ e)))))))))))

(defn walk
  "Exactly the same as clojure.walk/walk, except that it resolves promises"
  [inner outer form]
  (p/let [form form]
    (cond
      (list? form)
      (p/let [res (p/all (map #(p/let [e %] (inner e)) form))]
        (outer (apply list res)))

      (map-entry? form)
      (p/let [k (key form)
              v (val form)
              k (inner k)
              v (inner v)]
        (outer (MapEntry. k v nil)))

      (seq? form)
      (p/let [res (p/all (map #(p/let [e %] (inner e)) form))]
        (outer res))

      (record? form)
      (p/let [res (reduce (fn [r x]
                            (p/let [r r
                                    x x
                                    v (inner x)]
                              (conj r v)))
                          form form)]
        (outer res))

      (coll? form)
      (p/let [res (p/all (map #(p/let [e %] (inner e)) form))]
        (outer (into (empty form) res)))

      :else
      (outer form))))

(defn postwalk [fun form]
  (walk (partial postwalk fun) fun form))

(defn- string-node [string]
  (let [string (pr-str string)
        string (subs string 1 (-> string count dec))]
    (node-str/string-node string)))

(defn- transform-require [req opts]
  (p/let [node-file (->> req
                         :children
                         (map :lines)
                         (filter identity)
                         ffirst)
          file-contents (bundle node-file opts)]
    (assoc req :children [(token/token-node 'js/eval)
                          (whitespace/whitespace-node " ")
                          (string-node (str "(function () { eval("
                                            (pr-str file-contents)
                                            "); "
                                            "return entry; })()"))])))

(defn bundle-cljs [code opts]
  (p/let [elems (->> code
                     parser/parse-string-all
                     (postwalk (fn [a]
                                 (if (and (-> a :tag (= :list))
                                          (-> a :children first :value (= 'js/require)))
                                   (transform-require a opts)
                                   a))))]
    (str elems)))

(defn- js-namespace [bundlers]
  (let [accumulate-file #(swap! bundlers conj %)]
    {'require accumulate-file}))

(defn bundle! [sci-code]
  (let [bundlers (atom [])]
    (sci/eval-string sci-code {:namespaces {'js (js-namespace bundlers)}})))

(comment
  (def vega-code (resource/inline "vega_test.cljs"))
  (.then (bundle-cljs vega-code {})
         #(.writeFileSync
           (js/require "fs")
           "resources/vega-bundled.cljs"
           %))

  (.then (bundle-cljs vega-code {})
         println))
