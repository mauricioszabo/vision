(def vega (js/require "vega-embed"))

(defn vega-tag [edn]
  (let [container (doto (dom/create "div")
                        (dom/add-class! ["container"]))
        spec (clj->js edn)]
    (. vega default container spec)
    container))
