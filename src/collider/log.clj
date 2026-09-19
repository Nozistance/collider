(ns collider.log
  "Console logging."
  (:require [clojure.string :as str])
  (:import (java.io File FileOutputStream PrintStream PrintWriter
                    StringWriter)
           (java.time Instant LocalDateTime LocalTime ZoneId)
           (java.time.format DateTimeFormatter)
           (java.util Locale)))

(set! *warn-on-reflection* true)

(def ^:private ^DateTimeFormatter clock
  (DateTimeFormatter/ofPattern "HH:mm:ss"))

(def ^:private ^PrintStream console System/out)

(def ^:private file (atom nil))

(def ^:private ^DateTimeFormatter stamp
  (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss"))

(defn- ended-at ^String [^File f]
  (.format stamp (LocalDateTime/ofInstant
                   (Instant/ofEpochMilli (.lastModified f))
                   (ZoneId/systemDefault))))

(defn- rotated-name ^File [^File f]
  (File. (.getParentFile f) (str (ended-at f) ".log")))

(defn- rotate! [^File latest]
  (when (and (.exists latest) (pos? (.length latest)))
    (.renameTo latest (rotated-name latest))))

(defn to-file!
  "Sends every line to dir/latest.log as well as the console.
  The previous run's file moves aside under the time it ended."
  [dir]
  (when-not @file
    (let [d (File. ^String dir)
          latest (File. d "latest.log")]
      (.mkdirs d)
      (rotate! latest)
      (let [out (FileOutputStream. latest true)]
        (reset! file (PrintStream. out true "UTF-8"))))))

(defn- thread-name ^String []
  (let [n (.getName (Thread/currentThread))]
    (if (str/blank? n) "net" (str/replace n #"^collider-" ""))))

(defn- line ^String [level args]
  (str "[" (.format (LocalTime/now) clock) "] "
       "[" (thread-name) "/" level "]: "
       (str/join " " (map print-str args))))

(defn- emit! [^String s]
  (.println console s)
  (when-let [^PrintStream f @file] (.println f s)))

(defn info [& args]
  (emit! (line "INFO" args)))

(defn warn [& args]
  (emit! (line "WARN" args)))

(defn error [& args]
  (emit! (line "ERROR" args)))

(defn error-with
  "Logs the message and the throwable's stack trace after it."
  [msg ^Throwable t]
  (let [sw (StringWriter.)]
    (.printStackTrace t (PrintWriter. sw))
    (error msg (str t))
    (run! emit! (rest (str/split-lines (str sw))))))

(defn seconds
  "Returns a duration in nanoseconds as `(1.2s)`."
  ^String [^long nanos]
  (String/format Locale/ROOT "(%.1fs)" (to-array [(/ nanos 1e9)])))

(defn step
  "Logs what is about to happen, runs f, then logs how long it took."
  [doing done f]
  (info (str doing "..."))
  (let [t (System/nanoTime)
        v (f)]
    (info done (seconds (- (System/nanoTime) t)))
    v))

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
