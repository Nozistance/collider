(ns collider.log
  "Console logging."
  (:import (java.time LocalTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private ^DateTimeFormatter fmt (DateTimeFormatter/ofPattern "HH:mm:ss.SSS"))
(defn info
  "Prints the arguments after the time of day."
  [& args]
  (apply println (.format (LocalTime/now) fmt) args))

(defn human-bytes
  "Returns a byte count as a short string with a unit."
  ^String [n]
  (let [n (double n)]
    (loop [n n units ["B" "KiB" "MiB" "GiB" "TiB"]]
      (if (or (< n 1024.0) (empty? (rest units)))
        (if (= "B" (first units))
          (str (long n) " B")
          (String/format Locale/ROOT "%.1f %s"
                         (to-array [n (first units)])))
        (recur (/ n 1024.0) (rest units))))))
