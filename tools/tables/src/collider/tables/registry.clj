(ns collider.tables.registry
  "Reading packets, registries and blocks from the reports."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [collider.tables.value :refer [kw]]))

(set! *warn-on-reflection* true)

(defn- report-json [reports name]
  (json/read-str (slurp (io/file reports name))))

(defn- ids [entries]
  (into (sorted-map)
        (map (fn [[n e]] [(kw n) (get e "protocol_id")]))
        entries))

(defn packets [reports]
  (into {}
        (map (fn [[state dirs]]
               [(kw state)
                (into {}
                      (map (fn [[dir ps]] [(kw dir) (ids ps)]))
                      dirs)]))
        (report-json reports "packets.json")))

(defn registries [reports]
  (into (sorted-map)
        (map (fn [[name m]]
               [(str/replace name #"^minecraft:" "")
                (ids (get m "entries"))]))
        (report-json reports "registries.json")))

(defn- block [[name m] extra shaped]
  (let [{:strs [properties states definition]} m
        first-id (apply min (map #(get % "id") states))
        default (some #(when (get % "default") (get % "id")) states)
        props (into (sorted-map)
                    (map (fn [[p vs]] [(kw p) (mapv keyword vs)]))
                    properties)
        base {:first   first-id
              :default (or default first-id)
              :type    (kw (get definition "type"))}
        own (into (sorted-map) (merge base (get extra (kw name))))]
    [(kw name)
     (cond-> own
       (not (contains? shaped default)) (assoc :full-cube? true)
       (seq props) (assoc :props props))]))

(defn- run-ids [{:keys [runs]}]
  (into #{} (mapcat (fn [[lo hi]] (range lo (inc hi)))) runs))

(defn blocks
  "Returns the facts of every block."
  [reports extra shapes]
  (let [shaped (run-ids shapes)]
    (into (sorted-map)
          (map #(block % extra shaped))
          (report-json reports "blocks.json"))))
