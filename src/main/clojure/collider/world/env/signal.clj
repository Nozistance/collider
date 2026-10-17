(ns collider.world.env.signal
  "Redstone signal at a block, and game events a world hears."
  (:require [collider.world.block :as block]
            [collider.world.blocks.wire :as wire]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- known
  "Returns s, the signal of st toward side, or throws when the level
  decides it, which is not read yet."
  ^long [^long s st side]
  (if (neg? s)
    (throw (ex-info "signal the level decides is not read yet"
                    {:state st :block (block/block-of st)
                     :side side}))
    s))

(defn- wire-signal
  "Returns the signal wire st at p gives toward side d, which is its
  power upward and on the sides it runs to."
  ^long [chunks p ^long st d]
  (let [power (block/prop-long st :power)]
    (cond
      (or (= :down d) (zero? power)) 0
      (= :up d) power
      (not= :none (wire/side-of (wire/connection-state chunks p st)
                                (dir/opposite d)))
      power
      :else 0)))

(defn- given
  "Returns the signal or the direct signal st at p gives toward d.
  Wires give none while wires? is false."
  [chunks p st d wires? direct?]
  (if (wire/wire? st)
    (if wires? (wire-signal chunks p st d) 0)
    (known (if direct? (block/direct-signal st d) (block/signal st d))
           st d)))

(defn direct-signal
  "Returns the direct signal the block at p gives toward side."
  ^long [chunks p side]
  (long (given chunks p (chunk/at chunks p) side true true)))

(defn direct-signal-to
  "Returns the strongest direct signal the blocks around p give it.
  Wires give none while wires? is false."
  (^long [chunks p] (direct-signal-to chunks p true))
  (^long [chunks p wires?]
   (reduce (fn [^long m d]
             (let [q (dir/toward p d)
                   s (long (given chunks q (chunk/at chunks q) d wires?
                                  true))]
               (if (>= s 15) (reduced s) (max m s))))
           0 dir/six)))

(defn signal
  "Returns the signal the block at p gives the neighbour that looks at
  it toward side. A conductor gives the strongest direct signal around
  it as well. Wires give none while wires? is false."
  (^long [chunks p side] (signal chunks p side true))
  (^long [chunks p side wires?]
   (let [st (chunk/at chunks p)
         s (long (given chunks p st side wires? false))]
     (if (block/conductor? st)
       (max s (direct-signal-to chunks p wires?))
       s))))

(defn- neighbor-signal ^long [chunks p d wires?]
  (signal chunks (dir/toward p d) d wires?))

(defn has-neighbor-signal?
  "Returns true when a neighbour of p powers it."
  [chunks p]
  (boolean (some #(pos? (neighbor-signal chunks p % true)) dir/six)))

(defn best-neighbor-signal
  "Returns the strongest signal the neighbours of p give it. Wires
  give none while wires? is false."
  (^long [chunks p] (best-neighbor-signal chunks p true))
  (^long [chunks p wires?]
   (reduce (fn [^long m d]
             (let [s (neighbor-signal chunks p d wires?)]
               (if (>= s 15) (reduced s) (max m s))))
           0 dir/six)))

(defn game-event
  "Returns the deltas of a game event of kind at pos, which source
  gave. No listener hears it yet."
  [kind pos source]
  [[:game-event kind pos source]])
