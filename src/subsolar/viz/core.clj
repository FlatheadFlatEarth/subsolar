(ns subsolar.viz.core
  (:require [clojure.java.io :as io]
            [quil.core :as q :refer [with-rotation with-translation]]))

(def background-color 50)
(def window-width 1280)
(def window-height 720)

(def positions
  {
   :sun   [300 150]
   :earth [300 500]
   })

(def sizes
  {
   :person [29 75]
   :earth [200 200]
   :sun [200 200]
   })


(defn setup []
  (q/frame-rate 30)
  (q/background background-color)
  (q/stroke 255)
  (q/stroke-weight 2)
  (q/set-state!
   :image/earth    (q/load-image "resources/img/earth.png" )
   :image/person   (q/load-image "resources/img/person.png")
   :image/sun      (q/load-image "resources/img/sun.png"   )))

(defn draw-person []
  (let [im (q/state :image/person)
        [width height] (:person sizes)
        leg-length 35]
    (when (q/loaded? im)
      (q/image im
               (- (/ width 2))
               (- (- (/ height 2))
                  (/ (-> sizes
                         :earth
                         last)
                     2)
                  leg-length)))))
(defn draw-earth []
  (q/stroke 0 255 0)
  ;;(q/rect 50 50 50 50)
  (with-translation [300 500]
    (with-rotation [(/ (q/frame-count) 30)]
      (let [im (q/state :image/earth)
            [width height] (:earth sizes)]
        (when (q/loaded? im)
          (q/image im (- (/ width 2))
                      (- (/ height 2))))
        (draw-person)))))

(defn draw-sun []
  (let [im (q/state :image/sun)]
    (when (q/loaded? im)
      (q/image im 200 50))))

(defn draw-lines []
  (q/line
    300 150
    300 500))

(defn draw []
  (q/stroke 255)
  (q/fill background-color)
  (q/rect 0 0 window-width window-height)

  (q/fill 255)
  (q/text "hello world" 5 15)

  (draw-sun)
  (draw-earth)

  ;; (draw-lines)

  )

(defn on-close []
  (println "Viz on-close"))

(defn create-sketch []
  (q/sketch
    :title "Subsolar Visualization"
    :settings #(q/smooth 2)
    :setup #'setup
    :draw #'draw
    :on-close #'on-close
    :size [window-width
           window-height]))

(defonce sketch (create-sketch))
