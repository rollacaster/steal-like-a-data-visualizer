(ns steal-like-a-data-visualizer.core
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.sketch :as s]
            [quil.middleware :as md]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [goog.object :refer [get]]))

(def scroll-pos (r/atom 0))

(defn app []

  (r/create-class
   {:display-name "App"
    :component-did-mount
    (fn []
      (q/defsketch app-sketch
        :host "sketch"
        :setup s/setup
        :draw s/draw
        :update (fn [state] (s/update-state state @scroll-pos))
        :middleware [md/fun-mode]
        :size [s/width s/height])
      (.addEventListener (.getElementById js/document "scroll") "scroll"
                         #(do
                            (reset! scroll-pos (get (get % "target") "scrollTop")))))
    :reagent-render
    (fn []
      [:div.pa3.sans-serif
       [:h2 "Steal Like a Data Visualizer"]
       [:div#scroll {:style {:overflow "scroll" :width 549 :height 533}}
        [:div {:style {:height 2000 }}
         [:div#sketch {:style {:position "fixed" :top 83}}]]]])}))

(dom/render [app] (.getElementById js/document "app"))
