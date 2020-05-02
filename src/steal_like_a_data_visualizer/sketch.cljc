(ns steal-like-a-data-visualizer.sketch
  (:require [quil.core :as q]))

(defn setup []
  )

(defn update-state [state]
  )

(def margin {:top 10 :bottom 10 :left 10 :right 10})
(def width 500)
(def height 500)
(def circle-width (- (/ width 2) (:left margin) (:right margin)))
(def circle-height (- (/ height 2) (:top margin) (:bottom margin)))
(defn x [i] (+ (* circle-width (q/cos i))))
(defn y [i] (+ (* circle-height (q/sin i))))

(defn draw [state]
  (q/background 127)
  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/stroke 255)
  (doseq [i (range 0 q/TWO-PI 0.05)]
    (let [x1 (x i)
          y1 (y i)
          x2 (x (+ i 0.05))
          y2 (y (+ i 0.05))
          a (- x2 (x i))
          b (- y2 (y i))
          c (q/sqrt (+ (q/pow a 2) (q/pow b 2)))]
      (q/with-translation [x1 y1]
        (q/with-rotation [(if (= c 0.0)
                            0
                            (if (and (< y2 0) (> x2 0))
                              (q/acos (/ a c))
                              (if (< y2 0)
                                (- (q/acos (/ a c)))
                                (+ q/HALF-PI (q/acos (/ b c))))))]
          (q/line 10 0 6 4)
          (q/line 10 0 6 -4)
          (q/line 0 0 10 0))))))
