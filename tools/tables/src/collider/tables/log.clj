(ns collider.tables.log
  "Lines in the form of the vanilla server log."
  (:require [clojure.string :as str])
  (:import (java.io Console)
           (java.time LocalTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private clock (DateTimeFormatter/ofPattern "HH:mm:ss"))

(def ^:private terminal?
  (delay (boolean (some-> (System/console) Console/.isTerminal))))

(def ^:private open? (atom false))

(defn- line ^String [level args]
  (str "[" (DateTimeFormatter/.format clock (LocalTime/now)) "] "
       "[main/" level "]: " (str/join " " args)))

(defn- emit! [^String s end]
  (print (str (when @open? "\r\033[K") s end))
  (flush)
  (reset! open? (= "" end)))

(defn info [& args]
  (emit! (line "INFO" args) "\n"))

(defn error [& args]
  (emit! (line "ERROR" args) "\n"))

(defn status!
  "Shows a line that the next line replaces, in a terminal only."
  [& args]
  (when @terminal?
    (emit! (line "INFO" args) "")))

(defn begin! [& args]
  (apply (if @terminal? status! info) args))

(defn seconds
  "Returns a duration in nanoseconds as `(1.2s)`."
  ^String [^long nanos]
  (String/format Locale/ROOT "(%.1fs)" (to-array [(/ nanos 1e9)])))

(defn- unit-str ^String [^double n unit]
  (if (= "B" unit)
    (str (long n) " B")
    (String/format Locale/ROOT "%.1f %s" (to-array [n unit]))))

(defn human-bytes
  "Returns a byte count in the largest unit it fills."
  ^String [n]
  (loop [n (double n) units ["B" "KiB" "MiB" "GiB" "TiB"]]
    (if (or (< n 1024.0) (empty? (rest units)))
      (unit-str n (first units))
      (recur (/ n 1024.0) (rest units)))))
