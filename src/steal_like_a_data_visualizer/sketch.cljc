(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.vector :as v]))

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 533)
(def height 533)
(def r (- 200 (:left margin) (:right margin)))
(defn x [r phi] (* r (Math/cos phi)))
(defn y [r phi] (* r (Math/sin phi)))

(def medium-band-idxs
  (set (repeatedly 400 #(rand-int 3142))))

(def big-band-idxs
  (set (take 20 medium-band-idxs)))

(def big-bands-targets (map (fn [[x y]] [x (- y 50)])
                            [[-20 0] [0 0] [20 0]
                             [-30 20] [-10 20] [10 20] [30 20]
                             [-40 40] [-20 40] [0 40] [20 40] [40 40]
                             [-30 60] [-10 60] [10 60] [30 60]
                             [-20 80] [0 80] [20 80]
                             [-10 100] [10 100]]))

(defn setup-band [idx location]
  {:acceleration [0 0]
   :idx idx
   :velocity [0 0]
   :location location
   :r 10.0
   :big-target (if (= idx 8.0)
                 (last big-bands-targets)
                 (when (big-band-idxs idx)
                   (let [big-i (some (fn [[find-i big-i]] (when (= big-i idx) find-i))
                                     (map-indexed (fn [idx val] [idx val])big-band-idxs))]
                     (when big-i (nth big-bands-targets big-i)))))
   :type (cond
            (= idx 8.0) :sylvan-esso
            (big-band-idxs idx) :big
            (medium-band-idxs idx) :medium
            :else :small)
   :maxspeed (cond (or (= idx 8.0) (> (q/random 1) 0.5)) 3.0
                   :else 1.5)
   :maxforce (cond
               (= idx 8.0) 1
               (big-band-idxs idx) 1
               (medium-band-idxs idx) 1
               :else 0.1)})

(def bands
  (for [i (range 0 q/TWO-PI 0.002)]
    (let [idx (int (* i 500))]
      (setup-band idx
                     [(x (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))
                      (y (+ (* 2.5 (mod idx 15)) r) (+ i (q/random -0.05 0.05)))]))))
(defn setup []
  (q/text-align :center)
  (q/text-size 12)
  (q/no-stroke)
  {:bands bands
   :medium-venue-opacity 0.0
   :small-venue-opacity 0.0})

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (v/add acceleration force)))

(defn update-band [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (v/add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (v/limit velocity maxspeed))
        (assoc :location (v/add location velocity))
        (assoc :acceleration (v/mult acceleration 0)))))

(defn seek [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/mult (v/normalize (v/sub target location)) maxspeed)
        steer (v/limit (v/sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn arrive [{:keys [location maxspeed velocity maxforce] :as vehicle} target]
  (let [desired (v/sub target location)
        d (v/mag desired)
        steer (v/limit (v/sub (v/mult (v/normalize desired) (if (< d 5) (q/map-range d 0 5 0 maxspeed) maxspeed))
                              velocity) maxforce)]
    (apply-force vehicle steer)))

(defn next-coord-in-circle [{[lx ly] :location} r]
  (let [phi (q/atan2 ly lx)]
    [(x r (+ phi 0.05))
     (y r (+ phi 0.05))]))

(defn venue-by-scroll-pos [scroll-pos]
  (cond
    (> scroll-pos 300) :big
    (> scroll-pos 200) :medium
    :else :small))

(defmulti update-target (fn [band scroll-pos] [(:type band) (venue-by-scroll-pos scroll-pos)]) :default [:default :default])

(defmethod update-target [:small :big] [band]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod (:idx band) 15)) r))))
(defmethod update-target [:small :small] [band]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod (:idx band) 15)) r))))
(defmethod update-target [:small :medium] [band]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod (:idx band) 15)) r))))

(defmethod update-target [:medium :small] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod idx 15)) r))))
(defmethod update-target [:medium :medium] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))
(defmethod update-target [:medium :big] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))

(defmethod update-target [:big :small] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod idx 15)) r))))
(defmethod update-target [:big :medium] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))
(defmethod update-target [:big :big] [band]
  (arrive band (:big-target band)))

(defmethod update-target [:sylvan-esso :small] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod idx 15)) r))))
(defmethod update-target [:sylvan-esso :medium] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))
(defmethod update-target [:sylvan-esso :big] [band]
  (arrive band (:big-target band)))

(defn update-state [{:keys [bands]} scroll-pos]
  (let [small-venue-opacity (q/constrain (q/map-range scroll-pos 100 150 0 255) 0 255)
        medium-venue-opacity (q/constrain (q/map-range scroll-pos 200 250 0 255) 0 255)
        big-venue-opacity (q/constrain (q/map-range scroll-pos 300 350 0 255) 0 255)]
    {:small-venue-opacity small-venue-opacity
     :medium-venue-opacity medium-venue-opacity
     :big-venue-opacity big-venue-opacity
     :bands
     (map
      (fn [band]
        (-> band
            (update-target scroll-pos)
            update-band))
      bands)}))

(defn drop-every-n [n col]
  (keep-indexed
    (fn [index item]
      (if
        (not= 0 (mod (inc index) n))
          item
          nil))
    col))

(defmulti draw-band (fn [band _ _] (:type band)))

(defmethod draw-band :sylvan-esso [{[x y] :location} medium-venue-opacity big-venue-opacity]
  (q/stroke nil)
  (let [a (q/map-range medium-venue-opacity 0 255 255 0)]
    (q/fill 255 a))
  (q/text "Sylvan Esso" x (- y 8))
  (let [r (if (> big-venue-opacity 0)
            (q/map-range big-venue-opacity 0 255 174 255)
            (q/map-range medium-venue-opacity 0 255 255 174))
        g (if (> big-venue-opacity 0)
            (q/map-range medium-venue-opacity 0 255 219 190)
            (q/map-range medium-venue-opacity 0 255 0 219))
        b (if (> big-venue-opacity 0)
            (q/map-range medium-venue-opacity 0 255 71 210)
            (q/map-range medium-venue-opacity 0 255 200 71))
        a (q/map-range medium-venue-opacity 0 255 255 100)]
    (q/fill r g b a))
  (let [size (if (>= big-venue-opacity 0)
                 (q/map-range big-venue-opacity 0 255 5 20)
                 (q/map-range medium-venue-opacity 0 255 4 5))]
    (q/ellipse x y size size)))

(defmethod draw-band :small [{[x y] :location} medium-venue-opacity _]
  (q/stroke nil)
  (q/fill 255 (q/map-range medium-venue-opacity 0 255 100 50))
  (q/ellipse x y 3 3))

(defmethod draw-band :medium [{[x y] :location} medium-venue-opacity _]
  (q/stroke nil)
  (let [r (q/map-range medium-venue-opacity 0 255 255 174)
        g (q/map-range medium-venue-opacity 0 255 255 219)
        b (q/map-range medium-venue-opacity 0 255 255 71)
        a (q/map-range medium-venue-opacity 0 255 50 100)]
    (q/fill r g b a))
  (let [size (q/map-range medium-venue-opacity 0 255 3 5)]
    (q/ellipse x y size size)))

(defmethod draw-band :big [{[x y] :location} medium-venue-opacity big-venue-opacity]
  (q/stroke nil)
  (let [r (if (> big-venue-opacity 0)
            (q/map-range big-venue-opacity 0 255 174 255)
            (q/map-range medium-venue-opacity 0 255 255 174))
        g (if (> big-venue-opacity 0)
            (q/map-range big-venue-opacity 0 255 219 190)
            (q/map-range medium-venue-opacity 0 255 255 219))
        b (if (> big-venue-opacity 0)
            (q/map-range big-venue-opacity 0 255 71 210)
            (q/map-range medium-venue-opacity 0 255 255 71))
        a (q/map-range medium-venue-opacity 0 255 50 100)]
    (q/fill r g b a))
  (let [size (if (>= big-venue-opacity 0)
                 (q/map-range big-venue-opacity 0 255 5 20)
                 (q/map-range medium-venue-opacity 0 255 3 5))]
    (q/ellipse x y size size)))

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

(defn draw-medium-venue [opacity]
    (q/stroke 0)
    (q/fill 200 opacity)
    (draw-curved-text "MEDIUM" (* r 0.6))
    (q/stroke 200 opacity)
    (draw-dashed-circle (* r 0.6)))

(defn draw-big-venue [opacity]
    (q/stroke 0)
    (q/fill 200 opacity)
    (draw-curved-text "BIG" (* r 0.25))
    (q/stroke 200 opacity)
    (draw-dashed-circle (* r 0.25)))

(defn draw [{:keys [big-venue-opacity medium-venue-opacity small-venue-opacity bands]}]
  (q/background 0)
  (q/fill nil)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-small-venue small-venue-opacity)
  (draw-medium-venue medium-venue-opacity)
  (draw-big-venue big-venue-opacity)
  (q/fill 232 92 134 big-venue-opacity)
  (q/stroke 0)
  (doseq [band bands]
    (draw-band band medium-venue-opacity big-venue-opacity)))
