(ns collider.world.blocks.grow.sprout
  "Bone meal on bushes, dry grass, moss, lichen and rooted dirt."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]
            [collider.world.blocks.grow.common
             :refer [air-at? flagged]]
            [collider.world.blocks.moss :as moss]
            [collider.world.blocks.multiface :as multiface]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn roots-meal
  "Returns the hanging roots bone meal grows under rooted dirt."
  [chunks [_ y _ :as p] _st _roll]
  (when (and (chunk/in-range? (dec (long y)))
             (zero? (chunk/at chunks (dir/down p))))
    {:changes [[(dir/down p) (block/state :hanging-roots)]]}))

(defn lichen-meal
  "Returns the spread bone meal gives glow lichen."
  [chunks p st roll]
  (when-let [changes (multiface/spread-random chunks p st roll)]
    {:changes (flagged update/clients changes)}))

(defn carpet-meal
  "Returns the moss bone meal grows on a pale moss carpet."
  [chunks p st _roll]
  (when (= :true (:bottom (block/props-of st)))
    (when-let [topper (moss/carpet-topper chunks p (constantly true))]
      {:changes [[(dir/up p) topper]]})))

(defn hanging-moss-meal
  "Returns the moss bone meal adds to the end of hanging moss."
  [chunks p st _roll]
  (let [q (moss/hanging-end chunks p (block/block-of st))]
    (when (air-at? chunks q)
      {:changes [[q (block/with st :tip :true)]]})))

(defn- shuffled-sides [roll]
  (loop [pool dir/horizontal i 0 acc []]
    (if (= i 3)
      (into acc pool)
      (let [j (random/below (roll [:shuffle i]) (- 4 i))
            left (into (subvec pool 0 j) (subvec pool (inc j)))]
        (recur left (inc i) (conj acc (pool j)))))))

(defn- meal-room? [chunks q target]
  (and (air-at? chunks q) (support/supported? chunks q target)))

(defn- spread-meal [chunks p roll target]
  (let [q (->> (shuffled-sides roll)
               (map #(dir/toward p %))
               (filter #(meal-room? chunks % target))
               first)]
    (when q {:changes [[q target]]})))

(defn bush-meal
  "Returns the bush bone meal spreads beside a bush."
  [chunks p st roll]
  (spread-meal chunks p roll (block/state (block/block-of st))))

(defn short-dry-grass-meal
  "Returns short dry grass grown tall by bone meal."
  [_chunks p _st _roll]
  {:changes [[p (block/state :tall-dry-grass)]]})

(defn tall-dry-grass-meal
  "Returns the short dry grass bone meal spreads beside tall."
  [chunks p _st roll]
  (spread-meal chunks p roll (block/state :short-dry-grass)))
