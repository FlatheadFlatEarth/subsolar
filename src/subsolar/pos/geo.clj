(ns subsolar.pos.geo
  (:require [subsolar.util.time :refer [now]]
            ))

;; Constant height of the Sun above the Earth
(def sun-height 100)
(def full-revolution (* 2 Math/PI))
(def hours-in-day 24)
(def minutes-in-day (* 24 60))
(def days-in-year 365)

(def equator-to-tropic-distance 1.234)
(def north-pole-to-equator-distance 3.456)

(defn sun-dist-from-north-pole
  "Depending on the day of the year, the sun sits at a sinusoidal distance away from the Noth Pole, between the Tropic of Cancer and the Tropic of Capricorn"
  [dt]
  (let [days-since-equinox 0]
    (+ north-pole-to-equator-distance
       (* (Math/sin (* full-revolution
                       (/ days-since-equinox
                          days-in-year)))
          equator-to-tropic-distance))))

(defn rotational-sun-position
  "Every 24h, the sun returns to its current position.
  2pi / 24h"
  [dt]
  (let [minutes-elapsed (+ (* (.getHour dt) 60)
                           (.getMinute dt))]
    (* full-revolution
       (/ minutes-elapsed
          minutes-in-day))))

(defn sun-pos []
  (let [t     (now)
        d     (sun-dist-from-north-pole t)
        theta (rotational-sun-position t)]
    {:y sun-height
     :x (* d (Math/cos theta))
     :z (* d (Math/sin theta))}))
