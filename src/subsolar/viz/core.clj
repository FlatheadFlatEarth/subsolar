(ns subsolar.viz.core
  (:require [clojure.java.io :as io]
            [quil.core :as q :refer [
                                     fill
                                     rect
                                     frame-rate
                                     stroke-weight
                                     radians
                                     degrees
                                     state-atom
                                     sketch
                                     smooth
                                     set-state!
                                     arc background cos sin tan image loaded? load-image state stroke stroke-weight line text with-rotation with-translation PI
                                     ]]
            ))

(def background-color 50)
(def window-width 1280)
(def window-height 720)

(def positions
  {:earth [300 500]
   :sun   [300 150]})

(def sizes
  {:earth [200 200]
   :person [75 29]
   :sun [200 200]})

(defn setup []
  (frame-rate 30)
  (background background-color)
  (stroke 255)
  (stroke-weight 2)
  (set-state!
   :earth/rotation 0.1
   :image/earth    (load-image "resources/img/earth.png" )
   :image/person   (load-image "resources/img/person.png")
   :image/sun      (load-image "resources/img/sun.png"   )))

(defn draw-person []
  (let [im (state :image/person)
        [width height] (:person sizes)
        leg-length 35]
    (when (loaded? im)
      (image im
               (/ (-> sizes
                      :earth
                      last)
                  2)
               (- (/ height 2))))))

(defn draw-earth []
  (stroke 0 255 0)
  (with-translation [300 500]
      (with-rotation [(state :earth/rotation)]
      (let [im (state :image/earth)
            [width height] (:earth sizes)]
        (when (loaded? im)
          (image im (- (/ width 2))
                      (- (/ height 2))))
        (draw-person)))))

(defn draw-sun []
  (let [im (state :image/sun)]
    (when (loaded? im)
      (image im 200 50))))

(defn feet-location []
  (stroke 0 0 50)
  (let [[epx epy] (:earth positions)
        h (/ (-> sizes :earth first) 2)
        angle (state :earth/rotation)]
    [(+ epx
        (* h (cos angle)))
     (+ epy
        (* h (sin angle)))]))

(defn draw-distance-to-core []
  (stroke 50 250 0)
  (let [[epx epy] (:earth positions)
        [fx fy] (feet-location)
        angle (state :earth/rotation)]
    (with-translation [epx epy]
      (with-rotation [(+ angle (/ PI 2))]
        (text "Distance to Earth's Center" 10 -45)))
    (line epx epy fx fy)))

(defn draw-intersolar-distance []
  (stroke 200 0 0)
  ;(text "Intersolar Distance" 300 150)
  (line 300 150 300 500))

(defn new-intersolar-dist []
  (let [
        dist-to-sun 1234
        rotation-since-overhead (/ PI 4)
        observed-angle (radians 46)
        delta-y-from-rotation 100
        ]
    (/ (+ dist-to-sun delta-y-from-rotation)
       (sin (+ rotation-since-overhead
                 observed-angle)))))

(defn draw-angle-to-sun []
  (stroke 200 0 0)
  (let [[spx spy] (:sun positions)
        [fx fy] (feet-location)

        ;; angle
        ]
    (line spx spy fx fy)

    (arc 100 100 50 50 3.14 6.28)

    (with-translation [fx fy]
      (with-rotation [(+ (state :earth/rotation)
                         (/ PI 2))]
        (text "Sun angle: "
                -90
                -5
                ;;(+ fx 5)
                ;;(- fy 5)
                )))))

(defn draw-tangent-line []
  (stroke 0 200 0)
  (let [[epx epy] (:earth positions)
        [esx esy] (:earth sizes)
        angle (state :earth/rotation)]
    ;; Perpendicular slopes must be opposite reciprocals of each other:  m1 * m2 = –1
    (with-translation [300 500]
      (with-rotation [(+ (state :earth/rotation)
                         (/ PI 2))]
        (text "Tangent Line" 70 -105))
      (with-rotation [(state :earth/rotation)]
        ;;(tan angle)
        (line 100 -150 100 150)))))

(defn draw-lines []
  (stroke 66)

  (draw-intersolar-distance)
  (draw-tangent-line)
  (draw-angle-to-sun)
  (draw-distance-to-core)
  )

(defn draw []
  (stroke 255)
  (fill background-color)
  (rect 0 0 window-width window-height)

  (fill 255)

  (draw-sun)
  (draw-earth)
  (draw-lines)

  ;;(swap! (state-atom) update :earth/rotation #(+ 0.012 %))
  )

(defn on-close []
  (println "Viz on-close"))

(defn create-sketch []
  (sketch
    :title "Subsolar Visualization"
    :settings #(smooth 2)
    :setup #'setup
    :draw #'draw
    :on-close #'on-close
    :size [window-width
           window-height]))

(defonce my-sketch (create-sketch))
