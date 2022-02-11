(ns subsolar.core)

(defn calc-equation-of-time

  )

(defn subsolar-point
  ([sun-declination]
   (subsolar-point sun-declination (now)))
  ([sun-declination utc-time]
   (let [equation-of-time (calc-equation-of-time)]
     {:latitude sun-declination
      :longitude (* -15
                    (+ utc-time
                       -12
                       (/ equation-of-time 60)))})))
