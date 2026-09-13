(ns collider.data
  "The game data tables."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)
           (java.util Arrays HashMap List)))

(set! *warn-on-reflection* true)

(defn- load-edn [name]
  (with-open [r (io/reader (io/resource (str "mc/" name)))]
    (edn/read (PushbackReader. r))))

(def packets (load-edn "packets.edn"))
(def registries (load-edn "registries.edn"))
(def blocks (load-edn "blocks.edn"))
(def datapack (load-edn "datapack.edn"))
(def tags (load-edn "tags.edn"))
(def items (load-edn "items.edn"))
(def light (load-edn "light.edn"))
(def fire (load-edn "fire.edn"))
(def drops (load-edn "drops.edn"))
(def recipes (load-edn "recipes.edn"))
(def sounds (load-edn "sounds.edn"))
(defn max-stack
  "Returns how many of item fit in one stack."
  ^long [item]
  (long (get-in items [item :max-stack] 64)))

(defn jukebox-song
  "Returns the song a music disc plays, or nil when item is not one."
  [item]
  (get-in items [item :jukebox-song]))

(defn equip-slot
  "Returns the slot item is worn in, or nil when it is not worn."
  [item]
  (get-in items [item :equip]))

(defn dye-color
  "Returns the color item dyes with, or nil when it is not a dye."
  [item]
  (get-in items [item :dye]))

(defn pattern-tag
  "Returns the banner patterns item can apply, or nil when it applies none."
  [item]
  (get-in items [item :patterns]))

(defn compost
  "Returns the chance item raises a composter, or nil when it does not."
  [item]
  (get-in items [item :compost]))

(defn tag-values
  "Returns the entries of a tag of a registry, empty when there is no such
   tag."
  [registry tag]
  (get-in tags [registry tag] []))

(defn snake
  "Returns the name of keyword k with dashes turned into underscores."
  ^String [k]
  (.replace (name k) \- \_))

(defn wire
  "Returns the wire name of keyword k."
  ^String [k]
  (str (or (namespace k) "minecraft") ":" (snake k)))

(defn kebab
  "Returns the keyword for a name as it comes off the wire."
  [^String s]
  (let [s (.toLowerCase s)
        i (.indexOf s ":")
        ns (if (neg? i) "minecraft" (subs s 0 i))
        nm (.replace (if (neg? i) s (subs s (inc i))) \_ \-)]
    (if (= ns "minecraft") (keyword nm) (keyword ns nm))))

(defn packet-id
  "Returns the id of a packet of a connection state and direction. Throws when
   there is no such packet."
  ^long [state dir name]
  (or (get-in packets [state dir name])
      (throw (ex-info "unknown packet" {:state state :dir dir :name name}))))

(defn registry-id
  "Returns the id of an entry of a registry. Throws when there is no such
   entry."
  ^long [registry entry]
  (or (get-in registries [registry entry])
      (throw (ex-info "unknown registry entry" {:registry registry :entry entry}))))

(def ^:private datapack-index
  (into {}
        (map (fn [[registry entries]]
               [registry (into {} (map-indexed (fn [i e] [e (long i)])) entries)]))
        datapack))

(defn datapack-id
  "Returns the id of an entry the server sends to the client rather than one
   the client knows. Throws when there is no such entry."
  ^long [registry entry]
  (or (get (get datapack-index registry) entry)
      (throw (ex-info "unknown datapack entry" {:registry registry :entry entry}))))

(defn entry-id
  "Returns the id of an entry, from whichever kind of registry holds it."
  ^long [registry entry]
  (if (contains? registries registry)
    (registry-id registry entry)
    (datapack-id registry entry)))

(def ^:private by-id
  (into {}
        (map (fn [[registry entries]]
               [registry (into {} (map (fn [[k v]] [(long v) k])) entries)]))
        registries))

(defn entry-name
  "Returns the entry of a registry with the given id. Throws when there is no
   such entry."
  [registry ^long id]
  (if-let [m (get by-id registry)]
    (or (get m id)
        (throw (ex-info "unknown registry id" {:registry registry :id id})))
    (let [v (get datapack registry)]
      (when-not v (throw (ex-info "unknown registry" {:registry registry})))
      (when (or (neg? id) (>= id (count v)))
        (throw (ex-info "unknown registry id" {:registry registry :id id})))
      (nth v id))))

(defn- prop-order [b] (vec (keys (:props b))))
(defn- prop-sizes [b] (mapv #(count (get (:props b) %)) (prop-order b)))
(defn- state-count ^long [b] (reduce * 1 (map count (vals (:props b)))))
(defn- decode-props [b ^long offset]
  (let [order (prop-order b) sizes (prop-sizes b)]
    (loop [i 0, left offset, acc {}]
      (if (= i (count order))
        acc
        (let [tail (long (reduce * 1 (subvec sizes (inc i))))]
          (recur (inc i) (rem left tail)
                 (assoc acc (nth order i)
                            (nth (get (:props b) (nth order i)) (quot left tail)))))))))

(def block-state-count
  (long (reduce (fn [n [_ b]] (max n (+ (long (:first b)) (state-count b)))) 0 blocks)))

(def block-of-state
  (let [a (object-array block-state-count)]
    (doseq [[block b] blocks
            :let [from (long (:first b))]
            i (range (state-count b))]
      (aset a (+ from (long i)) block))
    a))

(defn- interned
  "Returns v with every value equal to one already seen replaced by that
   one, so equal values are shared."
  [^HashMap seen v]
  (if (vector? v)
    (let [v (mapv (fn [x] (interned seen x)) v)]
      (or (.get seen v) (do (.put seen v v) v)))
    v))

(defn- object-table [name]
  (let [a (object-array block-state-count)
        seen (HashMap.)]
    (doseq [[k v] (load-edn name)
            :when (< -1 (long k) block-state-count)]
      (aset a (int (long k)) (interned seen v)))
    a))

(defn- byte-table [name ^long default]
  (let [a (byte-array block-state-count)]
    (Arrays/fill a (byte default))
    (doseq [[k v] (load-edn name)
            :when (< -1 (long k) block-state-count)]
      (aset a (int (long k)) (byte (long v))))
    a))

(def ^"[Ljava.lang.Object;" shapes (object-table "shapes.edn"))
(def ^"[Ljava.lang.Object;" outlines (object-table "outlines.edn"))
(def ^bytes sturdy (byte-table "sturdy.edn" 63))
(def ^bytes sturdy-center (byte-table "sturdy-center.edn" 63))
(def ^bytes sturdy-rigid (byte-table "sturdy-rigid.edn" 63))
(def ^bytes flags (byte-table "flags.edn" 0))

(def default-props
  (into {}
        (map (fn [[block b]]
               [block (decode-props b (- (long (:default b)) (long (:first b))))]))
        blocks))

(defn info
  "Returns everything the tables hold about a block. Throws when there is no
   such block."
  [block]
  (or (get blocks block)
      (throw (ex-info "unknown block" {:block block}))))

(defn place-sound
  "Returns the sound of placing a block."
  [block]
  (get-in sounds [(:sound (info block)) :place]))

(defn open-sound
  "Returns the sound of a block opening, or of it closing."
  [block open?]
  (get (info block) (if open? :open :close)))

(defn by-hand?
  "Returns true if a block can be broken by hand."
  [block]
  (get (info block) :hand? true))

(defn- prop-index ^long [block-name prop vs v]
  (let [idx (.indexOf ^List vs v)]
    (when (neg? idx)
      (throw (ex-info "unknown property value" {:block block-name :prop prop :value v})))
    idx))

(defn- state-offset ^long [block-name b wanted defaults]
  (let [order (prop-order b) sizes (prop-sizes b)]
    (loop [i 0 id (long (:first b))]
      (if (= i (count order))
        id
        (let [prop (nth order i)
              idx (prop-index block-name prop (get (:props b) prop)
                              (get wanted prop (get defaults prop)))
              tail (long (reduce * 1 (subvec sizes (inc i))))]
          (recur (inc i) (long (+ id (* idx tail)))))))))

(defn state-id
  "Returns the global state id of a block: its default state, or the state
   with the wanted properties over the defaults."
  (^long [block-name] (long (:default (info block-name))))
  (^long [block-name wanted]
   (let [b (info block-name)]
     (if (empty? wanted)
       (state-id block-name)
       (state-offset block-name b wanted (get default-props block-name))))))

(defn state-block
  "Returns the block of a state id, or nil when there is no such state."
  [^long id]
  (when (< -1 id block-state-count) (aget ^objects block-of-state id)))

(defn state-props
  "Returns the block and the properties of a state id, or nil when there is no
   such state."
  [^long id]
  (when-let [block-name (state-block id)]
    (let [b (get blocks block-name)]
      [block-name (decode-props b (- id (long (:first b))))])))

