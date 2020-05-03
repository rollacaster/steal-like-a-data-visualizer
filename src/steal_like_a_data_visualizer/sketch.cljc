(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]
            [steal-like-a-data-visualizer.mover :as m]
            [steal-like-a-data-visualizer.vector :as v]))

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 500)
(def height 500)
(def r (- 200 (:left margin) (:right margin)))
(defn x [r phi] (+ (* r (Math/cos phi))))
(defn y [r phi] (+ (* r (Math/sin phi))))

(def movers
  (for [i (range 0 q/TWO-PI 0.1)]
    (let [r (- r (* (Math/random) 30))]
      (m/create-mover 100 [(x r i) (y r i)]))))

(defn setup-force [r phi]
  (let [x2 (x r (+ phi 0.05))
        y2 (y r (+ phi 0.05))
        a (- x2 (x r phi))
        b (- y2 (y r phi))
        c (Math/sqrt (+ (Math/pow a 2) (Math/pow b 2)))]
    {:x (x r phi)
     :y (y r phi)
     :angle (if (= c 0.0)
              0
              (cond
                (and (< y2 0) (> x2 0)) (Math/acos (/ a c))
                (< y2 0) (- (Math/acos (/ a c)))
                :else (+ q/HALF-PI (Math/acos (/ b c)))))}))

(def forces
  (flatten
   (map
    (fn [i]
      [(setup-force (- r 30) i)
       (setup-force (- r 15) i)
       (setup-force r i)])
    (range 0 q/TWO-PI 0.1))))

(defn setup []
  movers)

(defn update-state [state]
  (map
   (fn [mover]
     (-> mover
         (m/apply-force (let [active-force (some (fn [force]
                                                   (let [{[mx my] :location} mover
                                                         {:keys [x y]} force]
                                                     (when (< (q/dist x y mx my) 10) force))) forces)]
                          (if active-force
                            (v/rotate [5 0] (:angle active-force)) [0 0])))
         m/compute-position))
      state))



(defn draw-mover [{[x y] :location}]
  (q/text (str "[" (q/round x) " " (q/round y) "]") 0 -20)
  (q/ellipse x y 10 10))

(defn draw-force [{:keys [x y angle]}]
  (q/with-translation [x y]
     (q/with-rotation [angle]
       (q/line 10 0 6 4)
       (q/line 10 0 6 -4)
       (q/line 0 0 10 0))))

(defn draw [movers]
  (q/background 127)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/stroke 255)
  (doseq [force forces]
    (draw-force force))
  (doseq [mover movers]
    (draw-mover mover)))
