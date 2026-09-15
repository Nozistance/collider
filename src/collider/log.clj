(ns collider.log
  "Console logging."
  (:require [clojure.string :as str])
  (:import (java.io PrintStream)
           (java.time LocalTime)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private ^DateTimeFormatter clock (DateTimeFormatter/ofPattern "HH:mm:ss"))
(def ^:private ^PrintStream console System/out)

(defn- thread-name ^String []
  (let [n (.getName (Thread/currentThread))]
    (if (str/blank? n) "net" (str/replace n #"^collider-" ""))))

(defn- line ^String [level args]
  (str "[" (.format (LocalTime/now) clock) "] [" (thread-name) "/" level "]: "
       (str/join " " (map print-str args))))

(defn- emit! [^String s]
  (.println console s))

(defn info
  "Logs the arguments."
  [& args]
  (emit! (line "INFO" args)))

(defn warn
  "Logs the arguments as a warning."
  [& args]
  (emit! (line "WARN" args)))

(defn error
  "Logs the arguments as an error."
  [& args]
  (emit! (line "ERROR" args)))

(defn seconds
  "Returns a span of nanoseconds the way the server reports a duration."
  ^String [^long nanos]
  (String/format Locale/ROOT "(%.1fs)" (to-array [(/ nanos 1e9)])))

(defn step
  "Logs doing, calls f, logs done with how long it took, and returns the
   result of f."
  [doing done f]
  (info (str doing "..."))
  (let [t (System/nanoTime)
        v (f)]
    (info done (seconds (- (System/nanoTime) t)))
    v))

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
