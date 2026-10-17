(ns collider.world.env.signal
  "Redstone signal at a block, and game events a world hears."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- known ^long [^long s] (max s 0))

(defn direct-signal-to
  "Returns the strongest direct signal the blocks around p give it."
  ^long [chunks p]
  (reduce (fn [^long m d]
            (let [q (dir/toward p d)
                  s (known (block/direct-signal (chunk/at chunks q) d))]
              (if (>= s 15) (reduced s) (max m s))))
          0 dir/six))

(defn signal
  "Returns the signal the block at p gives the neighbour that looks at
  it toward side. A conductor gives the strongest direct signal around
  it as well."
  ^long [chunks p side]
  (let [st (chunk/at chunks p)
        s (known (block/signal st side))]
    (if (block/conductor? st)
      (max s (direct-signal-to chunks p))
      s)))

(defn- neighbor-signal ^long [chunks p d]
  (signal chunks (dir/toward p d) d))

(defn has-neighbor-signal?
  "Returns true when a neighbour of p powers it."
  [chunks p]
  (boolean (some #(pos? (neighbor-signal chunks p %)) dir/six)))

(defn best-neighbor-signal
  "Returns the strongest signal the neighbours of p give it."
  ^long [chunks p]
  (reduce (fn [^long m d]
            (let [s (neighbor-signal chunks p d)]
              (if (>= s 15) (reduced s) (max m s))))
          0 dir/six))

(defn game-event
  "Returns the deltas of a game event of kind at pos, which source
  gave. No listener hears it yet."
  [kind pos source]
  [[:game-event kind pos source]])
