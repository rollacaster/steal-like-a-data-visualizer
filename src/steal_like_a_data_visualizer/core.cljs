(ns steal-like-a-data-visualizer.core
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.sketch :as s]
            [quil.middleware :as md]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [goog.object :refer [get]]))

(def scroll-pos (r/atom 0))
(def max-width 533)
(def size (r/atom max-width))
(defonce sketch (r/atom nil))

(defn app []
  (r/create-class
   {:display-name "App"
    :component-did-mount
    (fn []
      (reset! size (- (.-innerWidth js/window) 50))
      (.addEventListener js/window "resize"
                           #(reset! size (.-innerWidth js/window)))
      (when (nil? @sketch)
        (q/defsketch app-sketch
          :host "sketch"
          :setup #(s/setup (Math/min @size max-width))
          :draw s/draw
          :update (fn [state] (s/update-state state @scroll-pos (Math/min @size max-width)))
          :middleware [md/fun-mode]
          :size [(Math/min @size max-width) (Math/min @size max-width)])
        (.addEventListener (.getElementById js/document "scroll") "scroll"
                           #(reset! scroll-pos (get (get % "target") "scrollTop")))
        (reset! sketch true)))
    :reagent-render
    (fn []
      [:div.sans-serif.bg-black.h-100.white
       [:div.mb4.pt4.ph4
        [:h1.mb3 "Steal Like a Data Visualizer"]
        [:p.mv0.mw8 "Improving your data visualization skills through theft may sound vicious at first, but it is a very effective learning method and hopefully a great tribute to the original creator."]]
       [:div
        [:div.mb4.ph4
         [:h2.white.mb2
          [:a.white.link.bb.b--white
           {:href "https://pudding.cool/2017/01/making-it-big/"}
           "The Unlikely Odds Of Making It Big"]]
         [:div.white.mv0
          [:div.mb4
           "by "                 
           [:a.white.link.bb.b--white
            {:href "https://pudding.cool/"}
            "The Pudding"]]
          [:div
           [:div.mb2
            [:span.mr1 "Blog:"]
            [:a.link.bb.b--white.white
             {:href "TODO"
              :target "_blank"}
             "TODO"]]
           [:div [:span.mr1 "Code:"]
            [:a.link.bb.b--white.white
             {:href "https://github.com/rollacaster/steal-like-a-data-visualizer"
              :target "_blank"}
             "https://github.com/rollacaster/steal-like-a-data-visualizer"]]]]]
        [:div#scroll.overflow-y-scroll.relative {:style {:max-width (+ max-width 50) :height max-width}}
         [:div.bg-black.ph4 {:style {:height (+ max-width 350) }}
          [:div#sketch {:style {:position "sticky" :top 0}}]]]]])}))

(dom/render [app] (.getElementById js/document "app"))
