(ns collider.tables.generate
  "Generating the game data tables from the vanilla server."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.tables.classes :as classes]
            [collider.tables.fetch :as fetch]
            [collider.tables.files :as files]
            [collider.tables.registry :as registry]
            [collider.tables.reports :as reports]
            [collider.tables.stamp :as stamp]
            [collider.tables.value :refer [unknown]]
            [collider.tables.progress :refer [progress! timed]])
  (:import (java.io File)
           (java.nio.file Path)))

(set! *warn-on-reflection* true)

(defn- write-edn! [dir k data]
  (let [f (io/file dir (str (name k) ".edn"))]
    (io/make-parents f)
    (with-open [w (io/writer f)]
      (binding [*out* w *print-length* nil *print-level* nil
                *print-namespace-maps* true]
        (pr data)
        (print "\n")))))

(def ^:private pack-keys
  [:props :pack :pack-tags :pack-reload :pack-components])

(defn- tables [from-class reports]
  (let [{:keys [props shapes]} from-class]
    (assoc (apply dissoc from-class pack-keys)
           :packets (registry/packets reports)
           :registries (registry/registries reports)
           :blocks (registry/blocks reports props shapes))))

(defn- pack-tables
  ([what known pack] (pack-tables what known pack ""))
  ([what known pack dir]
   (when-not (= (set known) (set (keys pack)))
     (throw (unknown what {:found (vec (keys pack))})))
   (map (fn [[path t]] [(str "pack/" dir path) t]) pack)))

(defn- tag-tables [tags]
  (when-not (= (set stamp/tags) (set (keys tags)))
    (throw (unknown "registries with tags in the pack"
                    {:found (vec (keys tags))})))
  (map (fn [[path t]] [(str "pack/tags/" path) t]) tags))

(defn- table-files [^File out]
  (let [pack (io/file out "pack")]
    (concat (filter File/.isFile (File/.listFiles out))
            (filter File/.isFile (file-seq pack)))))

(defn- table-path [^File out ^File f]
  (let [p (Path/.relativize (File/.toPath out) (File/.toPath f))]
    (str/join "/" (map str (seq p)))))

(defn- retire!
  "Deletes the tables in out that a full set no longer holds.
  Only edn files at the top and below pack/ are tables."
  [^File out]
  (let [known (into #{} (map #(str % ".edn"))
                    (conj stamp/files "stamp"))]
    (doseq [f (table-files out)
            :let [p (table-path out f)]
            :when (str/ends-with? p ".edn")
            :when (not (known p))]
      (io/delete-file f))))

(defn- pack-files [from-class]
  (concat (pack-tables "registries of the pack"
                       stamp/pack (:pack from-class))
          (pack-tables "reloadable data of the pack"
                       stamp/reloadable (:pack-reload from-class))
          (tag-tables (:pack-tags from-class))
          (pack-tables "components of the pack"
                       stamp/components (:pack-components from-class)
                       "components/")))

(defn- write-tables! [^File reports out from-class sha]
  (io/delete-file (io/file out "stamp.edn") true)
  (retire! (io/file out))
  (let [ts (concat (tables from-class reports)
                   (pack-files from-class))
        n (count ts)]
    (doseq [[i [k data]] (map-indexed vector ts)]
      (progress! {:event :progress :step :tables
                  :done (inc i) :total n})
      (write-edn! out k data))
    (write-edn! out :stamp (assoc (stamp/stamp) :server-sha1 sha))
    {:count n :dir (str out)}))

(defn generate!
  "Writes every table into out, from jar or from Mojang when nil."
  [{:keys [out cache jar]}]
  (let [bundle (fetch/fetch cache jar)
        sha (fetch/sha1 bundle)
        dir (reports/reports bundle (io/file cache sha))
        tmp (files/temp-dir "game")]
    (try (let [server (reports/game-jar bundle tmp)]
           (timed :tables
             #(write-tables! dir out
                (classes/read-classes bundle server) sha)))
         (finally (files/delete-tree! tmp)))))
