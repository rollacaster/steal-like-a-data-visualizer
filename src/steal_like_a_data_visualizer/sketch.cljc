(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]))

(derive ::medium-band ::small-band)
(derive ::big-band ::medium-band)
(derive ::sylvan-esso ::big-band)

(derive ::medium-venue ::small-venue)
(derive ::big-venue ::medium-venue)

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
(defn sub
  ([v] v)
  ([[x1 y1] [x2 y2]]
   [(- x1 x2) (- y1 y2)])
  ([v1 v2 & vs]
   (apply sub (sub v1 v2) vs)))
(defn mult [v1 n] (vector (* (first v1) n) (* (second v1) n)))
(defn div [[x y] n] (if (or (= n 0) (= n 0.0))
                      (vector x y)
                      (vector (/ x n) (/ y n))))
(defn normalize [[vx vy]]
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

(def sylvan-esso-id 8)
(def medium-band-idxs
  (set (repeatedly 400 #(rand-int 3142))))
(def big-band-idxs
  (conj (set (take 20 medium-band-idxs)) sylvan-esso-id))
(def big-bands-targets
  (zipmap big-band-idxs
          (let [x-step 25
                y-step 20]
            (map (fn [[x y]] [x (- y 50)])
                 (mapcat
                  (fn [y xs]
                    (map (fn [x] [x y]) xs))
                  (range 0 (inc (* 5 y-step)) y-step)
                  [(range (- x-step) (inc x-step) x-step)
                   (range (* 1.5 (- x-step)) (inc (* 1.5 x-step)) x-step)
                   (range (* 2 (- x-step)) (inc (* 2 x-step)) x-step)
                   (range (* 1.5 (- x-step)) (inc (* 1.5 x-step)) x-step)
                   (range (- x-step) (inc x-step) x-step)
                   (range (* 0.5 (- x-step)) (inc (* 0.5 x-step)) x-step)])))))
(defn setup-band [idx location]
  {:acceleration [0 0]
   :idx idx
   :velocity [0 0]
   :location location
   :type (cond
            (= idx sylvan-esso-id) ::sylvan-esso
            (big-band-idxs idx) ::big-band
            (medium-band-idxs idx) ::medium-band
            :else ::small-band)
   :maxspeed (if (> (q/random 1) 0.5) 3.0 1.5)
   :maxforce 1})
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
   :scroll-pos 0})

(defn apply-force [{:keys [acceleration] :as vehicle} force]
  (assoc vehicle :acceleration (add acceleration force)))

(defn update-vehicle [{:keys [velocity acceleration maxspeed location] :as vehicle}]
  (let [velocity (add velocity acceleration)]
    (-> vehicle
        (assoc :velocity (limit velocity maxspeed))
        (assoc :location (add location velocity))
        (assoc :acceleration (mult acceleration 0)))))

(defn seek [{:keys [maxspeed maxforce location velocity] :as vehicle} target]
  (let [desired (mult (normalize (sub target location)) maxspeed)
        steer (limit (sub desired velocity) maxforce)]
    (apply-force vehicle steer)))

(defn arrive [{:keys [maxspeed maxforce location velocity] :as vehicle} target]
  (let [desired (sub target location)
        d (apply q/mag desired)
        sclaed-desired (mult (normalize desired) (if (< d 5) (q/map-range d 0 5 0 maxspeed) maxspeed))
        steer (limit (sub sclaed-desired velocity) maxforce)]
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

(defn update-speed [{:keys [type maxspeed] :as band}]
  (assoc band :maxspeed (case type ::big-band 5 ::sylvan-esso 3 maxspeed)))

(defmulti update-target (fn [band scroll-pos] [(:type band) (venue-by-scroll-pos scroll-pos)]))
(defmethod update-target [::small-band ::small-venue] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 2.5 (mod idx 15)) r))))
(defmethod update-target [::medium-band ::medium-venue] [{:keys [idx] :as band}]
  (seek band (next-coord-in-circle band (+ (* 5 (mod idx 5)) (* 0.6 r)))))
(defmethod update-target [::big-band ::big-venue] [{:keys [idx] :as band}]
  (arrive band (big-bands-targets idx)))

(defn update-state [{:keys [bands]} scroll-pos]
  {:scroll-pos scroll-pos
   :bands (map #(-> %
                    update-speed
                    (update-target scroll-pos)
                    update-vehicle)
               bands)})

(defn transition-progress [scroll-pos venue]
  (q/constrain (q/map-range scroll-pos
                            (case venue ::small-venue 100 ::medium-venue 200 ::big-venue 300)
                            (case venue ::small-venue 150 ::medium-venue 250 ::big-venue 350)
                            0 1) 0 1))

(defmulti draw-band (fn [band scroll-pos] [(:type band) (venue-by-scroll-pos scroll-pos)]))
(prefer-method draw-band [::medium-band ::medium-venue] [::sylvan-esso ::small-venue])
(defmethod draw-band [::sylvan-esso ::small-venue] [{[x y] :location} scroll-pos]
  (let [a (q/lerp 255 0 (transition-progress scroll-pos ::small-venue))]
    (q/fill 255 a))
  (q/text "Sylvan Esso" x (- y 8))
  (let [r (q/lerp 255 174 (transition-progress scroll-pos ::small-venue))
        g (q/lerp 0 219 (transition-progress scroll-pos ::small-venue))
        b (q/lerp 200 71 (transition-progress scroll-pos ::small-venue))
        a (q/lerp 255 100 (transition-progress scroll-pos ::small-venue))]
    (q/fill r g b a))
  (let [size (q/lerp 4 5 (transition-progress scroll-pos ::small-venue))]
    (q/ellipse x y size size)))
(defmethod draw-band [::small-band ::small-venue] [{[x y] :location} scroll-pos]
  (q/fill 255 (q/lerp 100 50 (transition-progress scroll-pos ::small-venue)))
  (q/ellipse x y 3 3))
(defmethod draw-band [::medium-band ::medium-venue] [{[x y] :location} scroll-pos]
  (let [r (q/lerp 255 174 (transition-progress scroll-pos ::medium-venue))
        g (q/lerp 255 219 (transition-progress scroll-pos ::medium-venue))
        b (q/lerp 255 71 (transition-progress scroll-pos ::medium-venue))
        a (q/lerp 50 100 (transition-progress scroll-pos ::medium-venue))]
    (q/fill r g b a))
  (let [size (q/lerp 3 5 (transition-progress scroll-pos ::medium-venue))]
    (q/ellipse x y size size)))
(defmethod draw-band [::big-band ::big-venue] [{[x y] :location} scroll-pos]
  (let [r (q/lerp 174 255 (transition-progress scroll-pos ::big-venue))
        g (q/lerp 219 190 (transition-progress scroll-pos ::big-venue))
        b (q/lerp 71 210 (transition-progress scroll-pos ::big-venue))
        a (q/lerp 50 100 (transition-progress scroll-pos ::big-venue))]
    (q/fill r g b a))
  (let [size (q/lerp 5 20 (transition-progress scroll-pos ::big-venue))]
    (q/ellipse x y size size)))

(defn draw-venue [venue r scroll-pos]
  (q/stroke 0)
  (let [opacity (q/lerp 0 255 (transition-progress scroll-pos venue))]
    (q/fill 200 opacity)
    (let [str (case venue ::small-venue "SMALL VENUE" ::medium-venue "MEDIUM" ::big-venue "BIG")
          total-angle (/ (q/text-width str) r)]
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
      (q/end-shape))))

(defn draw [{:keys [bands scroll-pos]}]
  (q/background 0)
  (q/fill nil)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (draw-venue ::small-venue (+ r 10 ) scroll-pos)
  (draw-venue ::medium-venue (* r 0.6) scroll-pos)
  (draw-venue ::big-venue (* r 0.2) scroll-pos)
  (q/stroke nil)
  (doseq [band bands] (draw-band band scroll-pos)))
