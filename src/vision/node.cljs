(ns vision.node
  (:require [sci.core :as sci]
            [promesa.core :as p]
            ; [sci.impl.parser :as p]
            [edamame.core :as e]
            [rewrite-clj.zip :as zip]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.reader :as clj-reader]
            [rewrite-clj.node :as node]
            [rewrite-clj.node.seq :as seq]
            [rewrite-clj.node.stringz :as node-str]
            [rewrite-clj.node.token :as token]
            [rewrite-clj.node.whitespace :as whitespace]
            [clojure.walk :as walk]
            ["webpack" :as webpack]
            ["memfs" :refer [createFsFromVolume Volume]]))

; (def fs (js/require "fs"))

; (. fs readdir "node_modules" prn)
; (. fs stat "node_modules" #(def s %2))
; (. fs readlink "/tmp/log.lnk" #(def s2 %2))
; (.isFile s)

;; To implement a filesystem, you'll need:
;; readFile
;; stat
;; readlink
;; readdir
(defn bundle
  "Bundles a bunch of Javascript files with webpack, and return a single
Javascript content. Opts is:

:input-fs - a node.js compatible implementation containing, at least :readFile,
  :stat, :readlink, and :readdir"
  [first-file opts]
  (let [fake-fs (createFsFromVolume (Volume.))
        a (webpack (clj->js {:mode "production"
                             :entry first-file
                             :resolve {:fallback {:worker_threads false
                                                  :path "path-browserify"
                                                  :stream "stream-browserify"
                                                  :fs "memfs"
                                                  :constants "constants-browserify"
                                                  :os "os-browserify"
                                                  :child_process false
                                                  :crypto "crypto-browserify"
                                                  :https "https-browserify"
                                                  :http "http-browserify"
                                                  :vm "vm-browserify"}}
                             :output {:path "/tmp/dist"
                                      :library {:type "var"
                                                :name "entry"}
                                      :filename "bundle.js"}}))]
    (aset a "outputFileSystem" fake-fs)
    (when-let [input (:input-fs opts)] (aset a "inputFileSystem" (clj->js input)))
    (js/Promise. (fn [resolve fail]
                   (.run a (fn [err succ]
                             (prn :ERR err)
                             (prn :SUCC succ)
                             (if err
                               (fail err)
                               (resolve (.. fake-fs
                                            (readFileSync "/tmp/dist/bundle.js")
                                            toString)))))))))

#_
(.then (bundle "webpack" {})
       #(def res %))

#_
(count res)

#_
(.writeFileSync
 (js/require "fs")
 "/tmp/wpack.js"
 res)

#_
(bundle "vega-embed" {})

(def s
  "(def b (js/require \"vega-embed\"))
")

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

(defn- cut [a]
  (let [a (pr-str a)]
    (cond-> a (-> a count (> 50)) (subs 0 50))))
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


(defn- transform-require [req opts]
  (p/let [node-file (->> req
                         :children
                         (map :lines)
                         (filter identity)
                         ffirst)
          file-contents (bundle node-file opts)]
    (assoc req :children [(token/token-node 'js/eval)
                          (whitespace/whitespace-node " ")
                          (node-str/string-node file-contents)])))

(defn bundle-cljs [code opts]
  (p/let [elems (->> s
                     parser/parse-string-all
                     (postwalk (fn [a]
                                 (if (and (-> a :tag (= :list))
                                          (-> a :children first :value (= 'js/require)))
                                   (transform-require a opts)
                                   a))))]
    (str elems str)))

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
