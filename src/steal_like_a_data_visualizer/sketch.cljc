(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.vector :as v]))

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 500)
(def height 500)
(def r (- 200 (:left margin) (:right margin)))
(defn x [r phi] (* r (Math/cos phi)))
(defn y [r phi] (* r (Math/sin phi)))


(defn setup-vehicle [location]
  {:acceleration [0 0]
   :velocity [0 0]
   :location location
   :r 10.0
   :maxspeed 2.0
   :maxforce 0.1})

(def movers
  (for [i (range 0 q/TWO-PI 0.1)]
    (let [r (- r (* (Math/random) 30))]
      (setup-vehicle [(x r i) (y r i)]))))

(defn setup []
  movers)

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn seek [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/mult (v/normalize (v/sub target location)) maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn update-state [state]
  (map
   (fn [mover]
     (update-vehicle
      (seek mover
            (let [[lx ly] (:location mover)
                  phi (q/atan2 ly lx)]
              [(x (+ r (q/random -20 20)) (+ phi 0.05))
               (y (+ r (q/random -20 20)) (+ phi 0.05))]))))
   state))

(defn draw-mover [{[x y] :location}]
  (q/ellipse x y 10 10))

(defn draw [movers]
  (q/background 127)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/stroke 255)
  (doseq [mover movers]
    (draw-mover mover)))
