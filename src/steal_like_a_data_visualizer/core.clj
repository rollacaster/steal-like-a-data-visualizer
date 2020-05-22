(ns steal-like-a-data-visualizer.core
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.sketch :as s]
            [quil.middleware :as md]))

(defn run []
  (q/defsketch making-it-big
    :title "making-it-big"
    :settings #(q/smooth 2)
    :middleware [md/pause-on-error md/fun-mode]
    :setup s/setup
    :draw s/draw
    :update (fn [state] (s/update-state state 0))
    :display 1
    :features [:no-bind-output]
    :size [s/width s/height]))





