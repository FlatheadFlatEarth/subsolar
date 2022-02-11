(ns subsolar.pos.helio)

(def full-revolution (* 2 pi))
(def avg-orbital-distance 149.6) ;; avg-dist 149.60 million km

(defn elliptical-orbital-pos
  []

  )

(defn circular-orbital-pos
  "It's very similar to the elliptical orbit"
  [day-of-year]
  (let [d avg-orbital-distance
        theta (* full-revolution
                 (/ day-of-year
                    days-per-orbit))]
    {:x (* d (cos theta))
     :y (* d (sin theta))}))

(defn earth-orbital-pos
  "Model the alleged orbit of the Earth around the Sun, given a current time and producing the position in space relative to the Sun."
  []
  (circular-orbital-pos day))

(defn earth-current-rotation
  "Earth's current rotational orientation can be computed based on the time. Given a time, return an angle theta representing the direction that Greenwich, UK, is facing."
  []
  (* full-revolution
     (/ current-hour
        hours-per-day)))
