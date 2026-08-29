(ns collider.log
  (:import (java.time LocalTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private ^DateTimeFormatter fmt (DateTimeFormatter/ofPattern "HH:mm:ss.SSS"))
(defn info [& args]
  (apply println (.format (LocalTime/now) fmt) args))

(defn human-bytes ^String [n]
  (let [n (double n)]
    (loop [n n units ["B" "KiB" "MiB" "GiB" "TiB"]]
      (if (or (< n 1024.0) (empty? (rest units)))
        (if (= "B" (first units))
          (str (long n) " B")
          (String/format Locale/ROOT "%.1f %s"
                         (to-array [n (first units)])))
        (recur (/ n 1024.0) (rest units))))))
