(ns steal-like-a-data-visualizer.core
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.sketch :as s]
            [quil.middleware :as md]
            [reagent.core :as r]
            [reagent.dom :as dom]))

(defn app []
  (let [angle (r/atom 0)]
    (r/create-class
     {:display-name "App"
      :component-did-mount
      (fn []
        (q/defsketch app-sketch
          :host "sketch"
          :setup s/setup
          :draw (fn [state] (s/draw state @angle))
          :update s/update-state
          :middleware [md/fun-mode]
          :size [s/width s/height]))
      :reagent-render
      (fn []
        [:div.pa3.sans-serif
         [:h2 "Steal Like a Data Visualizer"]
         [:div.flex.items-center.mb3
          [:label.pr2 {:for "phi"} "Angle"]
          [:input.mr3 {:id "phi" :type "range" :min 0 :max 360
                       :value @angle :on-change #(reset! angle (.parseFloat js/window (.-value (.-target %))))}]
          [:span @angle]]
         [:div#sketch]])})))

(dom/render [app] (.getElementById js/document "app"))


