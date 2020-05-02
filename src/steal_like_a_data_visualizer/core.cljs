(ns steal-like-a-data-visualizer.core
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.sketch :as s]
            [quil.middleware :as md]
            [reagent.core :as r]
            [reagent.dom :as dom]))

(defn app []
  (r/create-class
   {:display-name "App"
    :component-did-mount
    (fn []
      (q/defsketch app-sketch
        :host "sketch"
        :setup s/setup
        :draw s/draw
        :update s/update-state
        :middleware [md/fun-mode]
        :size [s/width s/height]))
    :reagent-render
    (fn []
      [:div.pa3.sans-serif
       [:h2 "Steal Like a Data Visualizer"]
       [:div#sketch]])}))

(dom/render [app] (.getElementById js/document "app"))


