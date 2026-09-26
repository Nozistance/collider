(ns collider.world.blocks.geyser
  "Potent sulfur under water: the geyser above it and its countdown."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const water-reach
  "PotentSulfurBlock.ALLOWED_WATER_BLOCKS_ABOVE."
  4)

(def ^:private ^:const reach-per-water 6)

(defn phase
  "Returns the potent sulfur state of st, as :dormant."
  [^long st]
  (:potent-sulfur-state (block/props-of st)))

(defn- water-source? [^long st]
  (or (block/waterlogged? st) (block/water-source? st)))

(defn- passable?
  "Tells whether a geyser passes st, seen from below the cell.
  Scaffolding has no collision from below."
  [^long st]
  (or (= :water (block/block-of st))
      (= :scaffolding (block/type-of st))
      (empty? (block/collision-boxes st))))

(defn- through? [^long st]
  (and (water-source? st)
       (or (= :water (block/block-of st)) (passable? st))))

(defn source
  "Returns the cell above the water over the sulfur at pos, where
  its gas comes out, or nil when the water is too deep or capped."
  [chunks [x y z]]
  (loop [i 1]
    (when (<= i (inc water-reach))
      (let [q [x (+ (long y) i) z]
            st (chunk/at chunks q)]
        (cond
          (through? st) (recur (inc i))
          (passable? st) q)))))

(defn water-depth
  "Returns how many water blocks stand between pos and source q."
  ^long [[_ y _] [_ qy _]]
  (- (long qy) (long y) 1))

(defn reach
  "Returns how many cells above pos the geyser pushes through,
  up to six per water block."
  ^long [chunks [x y z] ^long depth]
  (let [top (* reach-per-water depth)
        at #(chunk/at chunks [x (+ (long y) 1 (long %)) z])]
    (loop [i 0]
      (cond
        (= i top) top
        (passable? (at i)) (recur (inc i))
        :else i))))

(defn- drawn
  "Returns a whole number from lo to hi fixed by pos and k."
  ^long [pos k ^long lo ^long hi]
  (let [r (random/of-key pos :geyser k)]
    (+ lo (long (* r (inc (- hi lo)))))))

(defn- countdown-from
  "Returns the countdown a geyser starts, by its phase and depth."
  ^long [pos ph ^long depth]
  (if (= :dormant ph)
    (+ (* 10 (dec depth)) (drawn pos 0 15 30))
    (+ (dec depth) (drawn pos 1 1 2))))

(defn counted
  "Returns the countdown of the geyser at pos after its step."
  ^long [pos ph ^long depth ^long countdown]
  (let [c (if (<= countdown 0)
            (countdown-from pos ph depth)
            countdown)]
    (if (pos? c) (dec c) c)))

(defn turned
  "Returns the state a geyser takes when its countdown runs out."
  ^long [^long st]
  (let [ph (if (= :dormant (phase st)) :erupting :dormant)
        props (block/props-of st)]
    (block/state (block/block-of st)
                 (assoc props :potent-sulfur-state ph))))
