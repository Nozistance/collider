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

(defonce ^:private file (atom nil))

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
  The log of the previous run moves aside under its end time."
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

(defn plain
  "Logs a line without the time and the thread, for the command line."
  [& args]
  (emit! (str/join " " (map print-str args))))

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

(defn name-of
  "Returns the name of var or function f for a log line."
  ^String [f]
  (if (var? f)
    (str (symbol f))
    (Compiler/demunge (.getName (class f)))))

(def ^:private ^:const quiet-ms 60000)

(defonce ^:private failures (atom {}))

(defn- tallied [^long now entry]
  (if (or (nil? entry) (<= quiet-ms (- now (long (:at entry)))))
    {:at now :n 0}
    (update entry :n inc)))

(defn failure!
  "Logs that unit failed with t, with the stack trace the first time.
  The same failure again within a minute is only counted. The count
  shows with the next line logged for it."
  [unit msg ^Throwable t]
  (let [now (System/currentTimeMillis)
        k [unit (class t)]
        [old new] (swap-vals! failures update k #(tallied now %))
        before (get old k)]
    (when (zero? (long (:n (get new k))))
      (if before
        (error msg (str t) "and" (:n before) "more times since")
        (error-with msg t)))))

(defn unit-failed!
  "Logs that unit f failed with t. The function msg-of takes the name
  of the unit and returns the message. Adds the failure to the atom
  failures when there is one."
  [failures f ^Throwable t msg-of]
  (let [unit (name-of f)]
    (failure! unit (msg-of unit) t)
    (some-> failures (swap! conj [unit t]))))

(defonce ^:private seen (atom #{}))

(defn once!
  "Logs args with the logging function f the first time key k comes."
  [k f & args]
  (when-not (@seen k)
    (swap! seen conj k)
    (apply f args)))

(defn seconds
  "Returns a duration of nanos as seconds in parentheses."
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
