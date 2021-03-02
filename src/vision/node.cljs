(ns vision.node
  (:require [sci.core :as sci]
            [promesa.core :as p]
            ; [sci.impl.parser :as p]
            [edamame.core :as e]
            [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.base :as zip-base]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.reader :as clj-reader]
            [rewrite-clj.node :as node]
            [rewrite-clj.node.seq :as seq]
            [rewrite-clj.node.stringz :as node-str]
            [rewrite-clj.node.token :as token]
            [rewrite-clj.node.whitespace :as whitespace]
            [rewrite-clj.zip.base :as zip-base]
            [clojure.walk :as walk]
            ["webpack" :as webpack]
            ["memfs" :refer [createFsFromVolume Volume]]))

;; To implement a filesystem, you'll need:
;; readFile
;; stat
;; readlink
;; readdir
#_
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
    (when-let [input (:input-fs opts)] (aset a "inputFileSystem" input))
    (js/Promise. (fn [resolve fail]
                   (.run a (fn [err succ] (if err
                                            (fail err)
                                            (resolve (.. fake-fs
                                                         (readFileSync "/tmp/dist/bundle.js")
                                                         toString)))))))))


(def s
  "(def b (js/require \"foo\"))

(def a (do (js/require \"lol\")) :foo/bar)")

(defn walk
  "Exactly the same as clojure.walk/walk, except that it resolves promises"
  [inner outer form]
  (p/let [form form]
    (cond
      (list? form)
      (p/let [res (p/all form)
              res (p/all (map inner res))]
        (outer (apply list res)))

      (map-entry? form)
      (p/let [k (key form)
              v (val form)
              k (inner k)
              v (inner v)]
        (outer (MapEntry. k v nil)))

      (seq? form)
      (p/let [res (p/all form)
              res (p/all (map inner res))]
        (outer res))

      (record? form)
      (outer (reduce (fn [r x]
                       (p/let [r r
                               v (inner x)]
                         (conj r v)))
                     form form))

      (coll? form)
      (p/let [res (p/all (map inner form))]
        (outer (into (empty form) res)))

      :else
      (outer form))))

(defn- cut [a]
  (let [a (pr-str a)]
    (cond-> a (-> a count (> 200)) (subs 0 200))))
#_
(p/let [elems (->> s
                   parser/parse-string-all
                   (postwalk (fn [a]
                               (if (and (-> a :tag (= :list))
                                        (-> a :children first :value (= 'js/require)))
                                 (transform-require a)
                                 a))))]
  (-> elems
      str
      println))


(defn postwalk [fun form]
  ; (walk (partial postwalk #(p/let [res %] (fun res))) fun form)
  (walk (partial postwalk fun) fun form))

#_
(postwalk #(js/Promise.resolve %) [:foo])
#_
(postwalk (fn [e] (if (int? e)
                    (js/Promise.resolve (str e))
                    e))
          {:foo [10 20 30] :bar 20})

#_
(let [r (e/reader s)
      ctx (sci/init {:load-fn (constantly "")
                     :readers identity
                     :features #{:clj :cljs}})]
  (sci/with-bindings {sci/ns @sci/ns}
    (sci/parse-next ctx r)))


(defn- transform-require [req]
  (let [node-file (->> req
                       :children
                       (map :lines)
                       (filter identity)
                       ffirst)]
    (assoc req :children [(token/token-node 'js/require*)
                          (whitespace/whitespace-node " ")
                          (node-str/string-node node-file)])))

#_
(p/let [elems (->> s
                   parser/parse-string-all
                   (postwalk (fn [a]
                               (prn :a a)
                               (if (and (-> a :tag (= :list))
                                        (-> a :children first :value (= 'js/require)))
                                 (transform-require a)
                                 a))))]
  (-> elems
      str
      println))
     ; str
     ; println)
       ; str))
; (node/list-node [])
; (node/replace-children)
; (-> f :children first zip/f)
; (zip/node f)

; (def a (zip/of-string "(def b (js/require 'foo))
;
; (def a (do (js/require \"lol\")) ::foo/bar)"))
; (->> (zip-base/child-sexprs a))
;      ; first)

(defn- js-namespace [bundlers]
  (let [accumulate-file #(swap! bundlers conj %)]
    {'require accumulate-file}))


(defn bundle! [sci-code]
  (let [bundlers (atom [])]
    (sci/eval-string sci-code {:namespaces {'js (js-namespace bundlers)}})))

#_
(bundle! "js/require")
#_
(bundle!
 vision.core-test/vega-code)

; const { createFsFromVolume, Volume } = require('memfs');
;
; const webpack = require('webpack');
;
; const fs = createFsFromVolume(new Volume());
; const compiler = webpack({
;                           /* options */})
; ;
;
; compiler.outputFileSystem = fs;
; compiler.run((err, stats) => {
;                               // Read the output later:
;                               const content = fs.readFileSync('...')});
; ;
; Note that this is what webpack-dev-middleware, used by webpack-dev-server and many other packages, uses to mysteriously hide your files but continue serving them up to the browser!
;
