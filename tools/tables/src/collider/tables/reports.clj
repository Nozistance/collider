(ns collider.tables.reports
  "Running the vanilla data generator for its reports."
  (:require [clojure.java.io :as io]
            [collider.tables.files :as files]
            [collider.tables.progress :refer [timed]])
  (:import (java.io File)
           (java.util.zip ZipFile)))

(set! *warn-on-reflection* true)

(defn game-jar
  "Unpacks the game jar that the server jar bundles into dir."
  ^File [^File bundle dir]
  (let [out (io/file dir "game.jar")]
    (with-open [zf (ZipFile/new bundle)]
      (->> (files/zip-names zf)
           (filter #(re-matches #"META-INF/versions/.*\.jar" %))
           first
           (#(files/unzip zf % out))))
    out))

(defn- generator-command ^String/1 [^File bundle]
  (let [java (str (System/getProperty "java.home") "/bin/java")]
    (into-array String [java
                        "-DbundlerMainClass=net.minecraft.data.Main"
                        "-jar" (File/.getAbsolutePath bundle)
                        "--reports"])))

(defn- generator-failed [log]
  (ex-info "the data generator failed"
           {:what "data generator failed"
            :why (str "Its output is in " log)}))

(defn- start ^Process [^File bundle ^File work ^File log]
  (-> (ProcessBuilder/new (generator-command bundle))
      (ProcessBuilder/.directory work)
      (ProcessBuilder/.redirectErrorStream true)
      (^[File] ProcessBuilder/.redirectOutput log)
      (ProcessBuilder/.start)))

(defn- run-generator! [^File bundle cache ^File dir]
  (let [work (files/temp-dir "reports")
        log (io/file cache "reports.log")]
    (io/make-parents log)
    (try (when-not (zero? (Process/.waitFor (start bundle work log)))
           (throw (generator-failed log)))
         (files/copy-tree! (io/file work "generated" "reports") dir)
         {:files (files/file-count dir)}
         (finally (files/delete-tree! work)))))

(defn reports
  "Returns the directory of the generator reports, made once."
  ^File [^File bundle cache]
  (let [dir (io/file cache "reports")
        found #(hash-map :files (files/file-count dir) :cached? true)]
    (if (File/.isFile (io/file dir "blocks.json"))
      (timed :reports found)
      (timed :reports #(run-generator! bundle cache dir)))
    dir))
