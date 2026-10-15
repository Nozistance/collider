(ns collider.persist.dump
  "The last ticks a debug dump holds and the file it writes."
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [collider.game.deltas :as deltas]
            [collider.log :as log]
            [collider.vec :as v])
  (:import (clojure.lang IPersistentMap PersistentArrayMap
                         PersistentHashMap PersistentQueue)
           (collider V3)
           (collider.game.deltas.record Deltas)))

(set! *warn-on-reflection* true)

(def empty-ring
  "A ring that holds no tick yet."
  PersistentQueue/EMPTY)

(defn recorded
  "Returns ring with tick t, its events and its deltas d at the end,
  the oldest ticks dropped past n."
  [ring n t events d]
  (let [r (conj ring {:tick t :events events :deltas d})]
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

(defn- unsaved [m]
  (cond-> m (= :store-chunk (:msg m)) (assoc :payload :omitted)))

(defn- entry [{:keys [deltas] :as e}]
  (cond-> (dissoc e :deltas)
    (instance? Deltas deltas)
    (assoc :world (deltas/world-of deltas)
           :entities (deltas/entities-of deltas)
           :out (mapv unsaved (deltas/out-of deltas)))))

(defn edn-of
  "Returns the ticks of ring as EDN text, plain data only."
  [ring]
  (binding [*print-length* nil *print-level* nil *print-meta* false]
    (pr-str (walk/prewalk plain (mapv entry ring)))))

(defn dump!
  "Writes the ticks of ring to file away from the caller."
  [ring file]
  (Thread/startVirtualThread
    #(try (io/make-parents file)
          (spit file (edn-of ring))
          (log/info "dumped" (count ring) "ticks to" file)
          (catch Throwable t
            (log/warn "could not dump to" file (ex-message t))))))
