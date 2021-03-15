(ns vision.core
  (:require [vision.node :as node]))

(def exports #js {:bundle_cljs node/bundle-cljs
                  :bundle_js node/bundle!})
