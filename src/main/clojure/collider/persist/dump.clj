(ns collider.persist.dump
  "The last ticks a debug dump holds and the file it writes."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [collider.game.deltas :as deltas]
            [collider.log :as log]
            [collider.vec :as v])
  (:import (clojure.lang IPersistentMap PersistentArrayMap
                         PersistentHashMap PersistentQueue)
           (collider V3)
           (collider.game.deltas.record Deltas)
           (java.io StringWriter Writer)))

(set! *warn-on-reflection* true)

(def empty-ring
  "A ring that holds no tick yet."
  PersistentQueue/EMPTY)

(def ^:private chunk-deltas #{:add-chunk :restore-chunk})

(defn- light-delta [d]
  (let [tag (nth d 0)]
    (cond (chunk-deltas tag) (assoc d 2 :omitted)
          (= :level-deltas tag) (update d 2 #(mapv light-delta %))
          :else d)))

(defn- light-event [ev]
  (if (= :chunk-loaded (nth ev 0)) (assoc ev 3 :omitted) ev))

(defn- unsaved [m]
  (cond-> m (= :store-chunk (:msg m)) (assoc :payload :omitted)))

(defn- light
  "Returns tick t of events and deltas d as a dump keeps it, the
  chunks in it left out."
  [t events d]
  (cond-> {:tick t :events (mapv light-event events)}
    (instance? Deltas d)
    (assoc :world (mapv light-delta (deltas/world-of d))
           :entities (deltas/entities-of d)
           :out (mapv unsaved (deltas/out-of d)))))

(defn recorded
  "Returns ring with tick t, its events and its deltas d at the end,
  the oldest ticks dropped past n. The chunks the tick loads, stores
  or receives stay out of it."
  [ring n t events d]
  (let [r (conj ring (light t events d))]
    (if (> (count r) (long n)) (pop r) r)))

(defn- clojure-map? [x]
  (or (instance? PersistentHashMap x) (instance? PersistentArrayMap x)
      (sorted? x)))

(defn- printable? [x]
  (or (nil? x) (boolean? x) (number? x) (string? x) (keyword? x)
      (symbol? x) (char? x) (uuid? x) (coll? x)))

(defn- plain [x]
  (cond
    (instance? V3 x) [(v/x x) (v/y x) (v/z x)]
    (record? x) (into {} x)
    (and (instance? IPersistentMap x) (not (clojure-map? x))) (into {} x)
    (some-> (class x) (.isArray)) (vec x)
    (printable? x) x
    :else (str "#" (.getSimpleName (class x)))))


(defn- pprinted [ring ^Writer w]
  (binding [*print-length* nil *print-level* nil *print-meta* false]
    (pprint/pprint (walk/prewalk plain (vec ring)) w)))

(defn edn-of
  "Returns the ticks of ring as EDN text laid out to read, plain data
  only."
  [ring]
  (let [w (StringWriter.)]
    (pprinted ring w)
    (str w)))

(defn dump!
  "Writes the ticks of ring to file away from the caller."
  [ring file]
  (Thread/startVirtualThread
    #(try (io/make-parents file)
          (with-open [w (io/writer file)] (pprinted ring w))
          (log/info "dumped" (count ring) "ticks to" file)
          (catch Throwable t
            (log/warn "could not dump to" file (ex-message t))))))
