(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.vector :as v]))

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 533)
(def height 533)
(def r (- 200 (:left margin) (:right margin)))
(defn x [r phi] (* r (Math/cos phi)))
(defn y [r phi] (* r (Math/sin phi)))

(def mediums
  (conj (set (repeatedly 400 #(rand-int 3142))) 8))

(defn setup-vehicle [idx location]
  {:acceleration [0 0]
   :idx idx
   :velocity [0 0]
   :location location
   :r 10.0
   :stage (if (mediums idx) "MEDIUM" "SMALL")
   :maxspeed (if (or (= idx 8.0)
                     (> (q/random 1) 0.5)) 3.0 1.5)
   :maxforce (if (mediums idx) 1 0.1)})

(def movers
  (for [i (range 0 q/TWO-PI 0.002)]
    (let [idx (int (* i 500))]
      (setup-vehicle idx
                     [(x (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))
                      (y (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))]))))
(defn setup []
  (q/text-align :center)
  (q/text-size 12)
  (q/no-stroke)
  {:movers movers
   :medium-opacity 0.0
   :small-venue-opacity 0.0})


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

(comment
  (q/with-sketch (q/get-sketch-by-id "sketch")
    (q/constrain (q/map-range 105 100 120 0 1) 0 1)))

(defn update-state [{:keys [movers]} scroll-pos]
  (let [medium-opacity (q/constrain (q/map-range scroll-pos 200 250 0 255) 0 255)]
    {:small-venue-opacity (q/constrain (q/map-range scroll-pos 100 150 0 255) 0 255)
     :medium-opacity medium-opacity
     :movers
     (map
      (fn [{[lx ly] :location :keys [idx] :as mover}]
        (update-vehicle
         (seek mover
               (let [phi (q/atan2 ly lx)
                     r (if (and (> medium-opacity 0.0) (= (:stage mover) "MEDIUM") (> scroll-pos 200))
                         (+ (* 5 (mod idx 5)) (* 0.6 r))
                         (+ (* 2.5 (mod idx 15)) r))]
                 [(x r (+ phi 0.05))
                  (y r (+ phi 0.05))]))))
      movers)}))

(defn draw-mover [{[x y] :location
                   :keys [idx stage]} medium-opacity]
  (q/stroke nil)
  (if (and (= idx 8.0) (= medium-opacity 0.0))
    (do
      (q/fill 255)
      (q/text "Sylvan Esso" x (- y 8))
      (q/fill 255 0 200)
      (q/ellipse x y 4 4))
    (do
      (if (and (> medium-opacity 0.0) (= stage "MEDIUM")) (q/fill 174 219 71 200) (q/fill 255 100))
      (if (and (> medium-opacity 0.0) (= stage "MEDIUM")) (q/ellipse x y 5 5) (q/ellipse x y 3 3)))))

(defn drop-every-n [n col]
  (keep-indexed
    (fn [index item]
      (if
        (not= 0 (mod (inc index) n))
          item
          nil))
    col))

(defn draw-dashed-circle [r]
  (doseq [dash (drop-every-n 2 (partition 3 (range 0 q/TWO-PI 0.01)))]
    (q/begin-shape)
    (doseq [i dash]
      (q/vertex
       (x (+ 40 r) i)
       (y (+ 40 r) i)))
    (q/end-shape)))

(defn draw-curved-text [str r]
  (let [total-angle (/ (q/text-width str) r)]
    (loop [str str
           arc-length 0]
      (let [c (first str)
            c-width (q/text-width c)
            theta
            (- (/ (+ arc-length (/ c-width 2)) r)
               (/ total-angle 2))]
        (q/push-matrix)
        (q/rotate theta)
        (q/translate 0 (- (+ 45 r)))
        (q/text c 0 0)
        (q/pop-matrix)
        (when (> (count str) 1)
          (recur (drop 1 str)
                 (+ arc-length c-width)))))))

(defn draw-small-venue [opacity]
  (q/stroke 0)
  (q/fill 200 opacity)
  (draw-curved-text "SMALL VENUE" (+ r 10))
  (q/stroke 200 opacity)
  (draw-dashed-circle (+ r 10)))

(defn draw [{:keys [medium-opacity small-venue-opacity movers]}]
  (q/background 0)
  (q/fill nil)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-small-venue small-venue-opacity)
  (when (> medium-opacity 0.0)
    (q/stroke 0)
    (q/fill 200 medium-opacity)
    (draw-curved-text "MEDIUM" (* r 0.6))
    (q/stroke 200 medium-opacity)
    (draw-dashed-circle (* r 0.6)))
  (doseq [mover movers]
      (draw-mover mover medium-opacity)))
