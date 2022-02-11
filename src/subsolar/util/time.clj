(ns subsolar.util.time
  (:import [java.time Instant LocalDateTime]))

(defn now []
  (LocalDateTime/now))
  ;;(Instant/now))
