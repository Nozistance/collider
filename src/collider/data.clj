(ns collider.data
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)
           (java.util List)))

(set! *warn-on-reflection* true)

(defn- load-edn [name]
  (with-open [r (io/reader (io/resource (str "mc/" name)))]
    (edn/read (PushbackReader. r))))

(def packets    (delay (load-edn "packets.edn")))
(def registries (delay (load-edn "registries.edn")))
(def blocks     (delay (load-edn "blocks.edn")))
(def datapack   (delay (load-edn "datapack.edn")))
(def tags       (delay (load-edn "tags.edn")))
(def shapes     (delay (load-edn "shapes.edn")))
(def sturdy     (delay (load-edn "sturdy.edn")))
(def sturdy-center (delay (load-edn "sturdy-center.edn")))
(def sturdy-rigid  (delay (load-edn "sturdy-rigid.edn")))
(def items      (delay (load-edn "items.edn")))
(def flags      (delay (load-edn "flags.edn")))
(def fire       (delay (load-edn "fire.edn")))
(def drops      (delay (load-edn "drops.edn")))

(defn max-stack
  "How many of the item go in one stack (64 unless the item says otherwise)."
  ^long [item]
  (long (get-in @items [item :max-stack] 64)))

(defn equip-slot
  "Equipment slot the item goes into (:head :chest :legs :feet :offhand ...), or nil."
  [item]
  (get-in @items [item :equip]))

(defn packet-id ^long [state dir name]
  (or (get-in @packets [state dir name])
      (throw (ex-info "unknown packet" {:state state :dir dir :name name}))))

(defn registry-id ^long [registry entry]
  (or (get-in @registries [registry entry])
      (throw (ex-info "unknown registry entry" {:registry registry :entry entry}))))

(defn datapack-id ^long [registry entry]
  (let [i (.indexOf ^List (get @datapack registry) entry)]
    (when (neg? i) (throw (ex-info "unknown datapack entry" {:registry registry :entry entry})))
    i))

(defn entry-id ^long [registry entry]
  (if (contains? @registries registry)
    (registry-id registry entry)
    (datapack-id registry entry)))

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

(def block-of-state
  (delay
   (persistent!
    (reduce (fn [m [block b]]
              (let [from (long (:first b))]
                (reduce (fn [m i] (assoc! m (+ from (long i)) block))
                        m (range (state-count b)))))
            (transient {}) @blocks))))

(def default-props
  (delay
   (into {}
         (map (fn [[block b]]
                [block (decode-props b (- (long (:default b)) (long (:first b))))]))
         @blocks)))

(defn info [block]
  (or (get @blocks block)
      (throw (ex-info "unknown block" {:block block}))))

(defn state-id
  (^long [block-name] (long (:default (info block-name))))
  (^long [block-name wanted]
   (let [b (info block-name)]
     (if (empty? wanted)
       (state-id block-name)
       (let [order (prop-order b) sizes (prop-sizes b)
             defaults (get @default-props block-name)]
         (loop [i 0 id (long (:first b))]
           (if (= i (count order))
             id
             (let [prop (nth order i)
                   vs   (get (:props b) prop)
                   v    (get wanted prop (get defaults prop))
                   idx  (.indexOf ^List vs v)
                   tail (long (reduce * 1 (subvec sizes (inc i))))]
               (when (neg? idx)
                 (throw (ex-info "unknown property value"
                                 {:block block-name :prop prop :value v})))
               (recur (inc i) (long (+ id (* idx tail))))))))))))

(defn state-props [^long id]
  (when-let [block-name (get @block-of-state id)]
    (let [b (get @blocks block-name)]
      [block-name (decode-props b (- id (long (:first b))))])))

(defn state-block [^long id]
  (get @block-of-state id))
