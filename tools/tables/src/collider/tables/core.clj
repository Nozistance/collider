(ns collider.tables.core
  "The command line of collider.tables."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.tables.fetch :as fetch]
            [collider.tables.generate :refer [generate!]]
            [collider.tables.log :as log]
            [collider.tables.progress :refer [*progress*]]
            [collider.tables.stamp :as stamp])
  (:import (clojure.lang ExceptionInfo)
           (java.io File))
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:private help
  [(str "Makes the game data tables of Minecraft " stamp/game
        " for Collider.")
   ""
   "Usage: java -jar collider-tables.jar <command> [options]"
   ""
   "generate       make the tables, downloads server.jar from Mojang"
   "  --out DIR    where to put them, data by default"
   "  --jar FILE   use this server.jar, no download"
   "  --cache DIR  where to keep server.jar and its reports,"
   (str "               OUT/" stamp/game " by default")
   "  --quiet      print errors only"
   "check DIR      tell if DIR has a full set of tables"
   "  --jar FILE   also tell if they came from this server.jar"
   "--version      print the game version and the table layout"
   ""
   "Example: java -jar collider-tables.jar generate --out ./data"])

(def ^:private valued {"--out" :out "--jar" :jar "--cache" :cache})

(defn- usage [& msg]
  {:command :usage :error (apply str msg)})

(defn- options [args]
  (loop [[a & more] args, m {:command :generate}]
    (cond
      (nil? a) m
      (= "--quiet" a) (recur more (assoc m :quiet? true))
      (not (valued a)) (usage "Unknown option " a)
      (empty? more) (usage "Option " a " needs a value")
      :else (recur (rest more) (assoc m (valued a) (first more))))))

(defn- defaults [{:keys [out cache] :or {out "data"} :as m}]
  (if (= :generate (:command m))
    (assoc m :out out :cache (or cache (str out "/" stamp/game)))
    m))

(defn- check-args [[dir & more]]
  (cond
    (or (nil? dir) (str/starts-with? dir "-"))
    (usage "check needs a directory")
    (empty? more) {:command :check :dir dir}
    (and (= "--jar" (first more)) (= 2 (count more)))
    {:command :check :dir dir :jar (second more)}
    :else (usage "check takes only --jar")))

(defn- parse [args]
  (let [[c & more] args]
    (defaults
      (cond
        (nil? c) (options more)
        (= "generate" c) (options more)
        (= "check" c) (check-args more)
        (#{"help" "--help" "-h"} c) {:command :help}
        (= "--version" c) {:command :version}
        (str/starts-with? c "-") (options args)
        :else (usage "Unknown command " c)))))

(def ^:private doing
  {:reports "Generating reports"
   :tables  "Generating tables"})

(defmulti ^:private render! :event)

(defmethod render! :default [_] nil)

(defmethod render! :begin [{:keys [step url]}]
  (if url
    (log/info "Fetching server.jar from" url)
    (log/begin! (str (doing step) "..."))))

(defn- percent [^long done ^long total]
  (str (quot (* 100 done) (max total 1)) "%"))

(defn- part-of [done total]
  (str "(" (log/human-bytes done) " of " (log/human-bytes total) ")"))

(defmethod render! :progress [{:keys [step done total]}]
  (case step
    :jar (log/status! "Downloading server.jar" (percent done total)
                      (part-of done total))
    :tables (log/status! "Generating tables" done "of" total)))

(defn- jar-done [{:keys [source bytes path took]}]
  (case source
    :mojang (str "Downloaded server.jar, " (log/human-bytes bytes)
                 " " (log/seconds took))
    :cached (str "Found server.jar in " path)
    :local (str "Using server.jar from " path)))

(defn- reports-done [{:keys [files cached? took]}]
  (if cached?
    (str "Found " files " report files")
    (str "Generated " files " report files " (log/seconds took))))

(defmethod render! :end [{:keys [step count dir took] :as m}]
  (log/info
    (case step
      :jar (jar-done m)
      :reports (reports-done m)
      :tables (str "Generated " count " tables into " dir " "
                   (log/seconds took)))))

(defmethod render! :done [{:keys [took]}]
  (log/info (str "Done " (log/seconds took) "!")))

(defmethod render! :error [{:keys [what why command]}]
  (log/error (str "**** " (str/upper-case (or what "failed")) "!"))
  (doseq [l (if (string? why) [why] why)] (log/error l))
  (when command (log/error command)))

(defmulti ^:private execute :command)

(defmethod execute :help [_]
  (run! println help)
  0)

(defmethod execute :version [_]
  (println (str "collider-tables for Minecraft " stamp/game
                ", table layout " stamp/layout))
  0)

(defmethod execute :usage [{:keys [error]}]
  (binding [*out* *err*]
    (println error)
    (println "See collider-tables help"))
  2)

(defn- failure [^Exception e]
  (merge {:what "table generation failed"
          :why (str "The exception was: " e)}
         (when (instance? ExceptionInfo e) (ex-data e))
         {:event :error}))

(defmethod execute :generate [{:keys [quiet?] :as opts}]
  (binding [*progress* (if quiet? (fn [_] nil) render!)]
    (when-not quiet?
      (log/info "Making the tables of Minecraft" stamp/game))
    (try (let [t (System/nanoTime)]
           (generate! opts)
           (*progress* {:event :done :took (- (System/nanoTime) t)}))
         0
         (catch Exception e
           (render! (failure e))
           1))))

(def ^:private how
  "Make them with: java -jar collider-tables.jar generate --out ")

(defn- other-jar [dir jar found]
  (cond
    (not (File/.isFile (io/file jar)))
    (str "There is no file at " jar)
    (not= (fetch/sha1 (io/file jar)) (:server-sha1 found))
    (str "The tables in " dir " were made from another jar than "
         jar)))

(defn- check-failure [{:keys [dir jar]}]
  (let [lack (stamp/missing dir)
        found (stamp/stamp-of dir)
        mark (select-keys found [:game :layout])]
    (cond
      (= (count lack) (inc (count stamp/files)))
      (str "There are no tables in " dir)
      (seq lack)
      (str "The tables in " dir " lack " (str/join ", " lack))
      (not= (stamp/stamp) mark)
      (str "The tables in " dir " are for " (:game found)
           " layout " (:layout found) ", not " stamp/game
           " layout " stamp/layout)
      jar (other-jar dir jar found))))

(defmethod execute :check [{:keys [dir] :as opts}]
  (if-let [why (check-failure opts)]
    (do (render! {:event :error :what "no game data" :why why
                  :command (str how dir)})
        1)
    (do (log/info "The tables in" dir "are complete")
        0)))

(defn -main [& args]
  (System/exit (execute (parse args))))
