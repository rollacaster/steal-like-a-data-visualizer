(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]))

(def width 533)
(def height 533)
(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def r (- 200 (:left margin) (:right margin)))

(defn x [r phi] (* r (Math/cos phi)))
(defn y [r phi] (* r (Math/sin phi)))

(defn add
  ([v] v)
  ([[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])
  ([v1 v2 & vs]
   (apply add (add v1 v2) vs)))
(defn mult [v1 n] (vector (* (first v1) n) (* (second v1) n)))
(defn div [[x y] n] (if (or (= n 0) (= n 0.0))
                      (vector x y)
                      (vector (/ x n) (/ y n))))
(defn normalize [[[vx vy]]]
  (let [m (q/mag vx vy)]
    (if (not (= m 0.0)) (div [vx vy] m) [vx vy])))
(defn limit [[x y] top]
  (if (> (q/mag x y) top)
    (mult (normalize [x y]) top)
    [x y]))

(defn drop-every-n [n col]
  (keep-indexed
    (fn [index item]
      (if
        (not= 0 (mod (inc index) n))
          item
          nil))
    col))

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
            (= idx 8.0) ::sylvan-esso
            (big-band-idxs idx) ::big-band
            (medium-band-idxs idx) ::medium-band
            :else ::small-band)
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
  (assoc vehicle :acceleration (add acceleration force)))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (limit velocity maxspeed))
        (assoc :location (add location velocity))
        (assoc :acceleration (mult acceleration 0)))))

(defn seek [{[x1 y1] :location
             [vx vy] :velocity
             :keys [maxspeed maxforce] :as vehicle} {[x2 y2] :target}]
  (let [[dx dy] (mult (normalize (q/dist x2 y2 x1 y1)) maxspeed)
        steer (limit (q/dist dx dy vx vy) maxforce)]
    (apply-force vehicle steer)))

(defn arrive [{[x1 y1] :location
               [vx vy] :velocity
               :keys [maxspeed maxforce] :as vehicle} {[x2 y2] :target}]
  (let [desired (q/dist x1 y1 x2 y2)
        d (apply q/mag desired)
        [tx ty] (mult (normalize desired) (if (< d 5) (q/map-range d 0 5 0 maxspeed) maxspeed))
        steer (limit (q/dist tx ty vx vy) maxforce)]
    (apply-force vehicle steer)))

(defn next-coord-in-circle [{[lx ly] :location} r]
  (let [phi (q/atan2 ly lx)]
    [(x r (+ phi 0.05))
     (y r (+ phi 0.05))]))

(defn venue-by-scroll-pos [scroll-pos]
  (cond
    (> scroll-pos 300) ::big-venue
    (> scroll-pos 200) ::medium-venue
    :else ::small-venue))

(derive ::sylvan-esso ::big-band)
(derive ::medium-band ::small-band)
(derive ::big-band ::medium-band)
(derive ::big-venue ::medium-venue)
(derive ::medium-venue ::small-venue)

(defmulti update-target (fn [band scroll-pos] [(:type band) (venue-by-scroll-pos scroll-pos)]))
(defmethod update-target [::small-band ::small-venue] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod idx 15)) r))))
(defmethod update-target [::medium-band ::medium-venue] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))
(defmethod update-target [::big-band ::big-venue] [band]
  (arrive band (:big-target band)))

(defn update-state [{:keys [bands]} scroll-pos]
  (let [small-venue-opacity (q/constrain (q/map-range scroll-pos 100 150 0 255) 0 255)
        medium-venue-opacity (q/constrain (q/map-range scroll-pos 200 250 0 255) 0 255)
        big-venue-opacity (q/constrain (q/map-range scroll-pos 300 350 0 255) 0 255)]
    {:small-venue-opacity small-venue-opacity
     :medium-venue-opacity medium-venue-opacity
     :big-venue-opacity big-venue-opacity
     :scroll-pos scroll-pos
     :bands (map #(update-vehicle (update-target % scroll-pos)) bands)}))

(defmulti draw-band (fn [band scroll-pos _ _ _] [(:type band) (venue-by-scroll-pos scroll-pos)]))
(defmethod draw-band [::sylvan-esso :small-venue] [{[x y] :location} medium-venue-opacity]
  (q/stroke nil)
  (let [a (q/map-range medium-venue-opacity 0 255 255 0)]
    (q/fill 255 a))
  (q/text "Sylvan Esso" x (- y 8))
  (let [r (q/map-range medium-venue-opacity 0 255 255 174)
        g (q/map-range medium-venue-opacity 0 255 0 219)
        b (q/map-range medium-venue-opacity 0 255 200 71)
        a (q/map-range medium-venue-opacity 0 255 255 100)]
    (q/fill r g b a))
  (let [size (q/map-range medium-venue-opacity 0 255 4 5)]
    (q/ellipse x y size size)))
(defmethod draw-band [::small-band ::small-venue] [{[x y] :location} medium-venue-opacity]
  (q/stroke nil)
  (q/fill 255 (q/map-range medium-venue-opacity 0 255 100 50))
  (q/ellipse x y 3 3))
(defmethod draw-band [::medium-band ::medium-venue] [{[x y] :location} medium-venue-opacity]
  (q/stroke nil)
  (let [r (q/map-range medium-venue-opacity 0 255 255 174)
        g (q/map-range medium-venue-opacity 0 255 255 219)
        b (q/map-range medium-venue-opacity 0 255 255 71)
        a (q/map-range medium-venue-opacity 0 255 50 100)]
    (q/fill r g b a))
  (let [size (q/map-range medium-venue-opacity 0 255 3 5)]
    (q/ellipse x y size size)))
(defmethod draw-band [::big-band ::big-venue] [{[x y] :location} medium-venue-opacity big-venue-opacity]
  (q/stroke nil)
  (let [r (q/map-range big-venue-opacity 0 255 174 255)
        g (q/map-range big-venue-opacity 0 255 219 190)
        b (q/map-range big-venue-opacity 0 255 71 210)
        a (q/map-range medium-venue-opacity 0 255 50 100)]
    (q/fill r g b a))
  (let [size (q/map-range big-venue-opacity 0 255 5 20)]
    (q/ellipse x y size size)))

(defn draw-venue [text r opacity]
  (q/stroke 0)
  (q/fill 200 opacity)
  (let [total-angle (/ (q/text-width text) r)]
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
                 (+ arc-length c-width))))))
  (q/stroke 200 opacity)
  (doseq [dash (drop-every-n 2 (partition 3 (range 0 q/TWO-PI 0.01)))]
    (q/begin-shape)
    (doseq [i dash]
      (q/vertex
       (x (+ 40 r) i)
       (y (+ 40 r) i)))
    (q/end-shape)))

(defn draw [{:keys [big-venue-opacity medium-venue-opacity small-venue-opacity bands
                    scroll-pos]}]
  (q/background 0)
  (q/fill nil)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-venue "SMALL VENUE" (+ r 10 ) small-venue-opacity)
  (draw-venue "MEDIUM" (* r 0.6) medium-venue-opacity)
  (draw-venue "BIG" (* r 0.25) big-venue-opacity)
  (q/fill 232 92 134 big-venue-opacity)
  (q/stroke 0)
  (doseq [band bands]
    (draw-band band scroll-pos medium-venue-opacity big-venue-opacity)))
