(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.vector :as v]))

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 533)
(def height 533)
(def r (- 220 (:left margin) (:right margin)))
(defn x [r phi] (* r (Math/cos phi)))
(defn y [r phi] (* r (Math/sin phi)))


(defn setup-vehicle [idx location]
  {:acceleration [0 0]
   :idx idx
   :velocity [0 0]
   :location location
   :r 10.0
   :maxspeed (if (> (q/random 1) 0.5) 1.5 3.0)
   :maxforce 0.1})

(def movers
  (for [i (range 0 q/TWO-PI 0.002)]
    (let [idx (int (* i 500))]
      (setup-vehicle idx
                     [(x (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))
                      (y (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))]))))

(defn setup []
  (q/text-align :center)
  (q/no-stroke)
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
  (into [] (comp (map
               (fn [{[lx ly] :location :keys [idx] :as mover}]
                 (update-vehicle
                  (seek mover
                        (let [phi (q/atan2 ly lx)]
                          [(x (+ (* 2.5 (mod idx 15)) r) (+ phi 0.05))
                           (y (+ (* 2.5 (mod idx 15)) r) (+ phi 0.05))]))))))
        state))

(defn draw-mover [{[x y] :location}]
  (q/fill 255 100)
  (q/ellipse x y 3 3))

(defn draw [movers]
  (q/background 0)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (doseq [mover movers]
    (draw-mover mover)))
