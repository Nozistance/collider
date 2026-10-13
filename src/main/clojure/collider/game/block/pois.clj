(ns collider.game.block.pois
  "Points of interest of a level by section, each type held as the
  cells in the order they came and the width of the table of the hash
  set that lists them."
  (:require [collider.cell :as cell]
            [collider.data :as data]
            [collider.hash-order :as hash-order]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def none {})

(def ^:private rod-types #{:lightning-rod :weathering-lightning-rod})

(defn- type-of [st]
  (when (contains? rod-types (block/type-of st)) :lightning-rod))

(def ^:private ^:table any
  (delay (boolean-array (map (comp some? type-of)
                             (range (data/block-state-count))))))

(defn- section-of [[x y z]]
  [(bit-shift-right (long x) 4) (bit-shift-right (long y) 4)
   (bit-shift-right (long z) 4)])

(defn- added [{:keys [cells cap] :or {cells [] cap 16}} p]
  (if (some #{p} cells)
    {:cells cells :cap cap}
    {:cells (conj cells p)
     :cap (hash-order/grown cap (map cell/hashed cells)
                            (cell/hashed p))}))

(defn- removed [s p]
  (update s :cells #(into [] (remove #{p}) %)))

(defn- changed-at [pois [p old st]]
  (let [a (type-of old) b (type-of st) sec (section-of p)]
    (if (= a b)
      pois
      (cond-> pois
        (and a (get-in pois [sec a])) (update-in [sec a] removed p)
        b (update-in [sec b] added p)))))

(defn changed
  "Returns pois after the block changes [pos old state] in turn."
  [pois changes]
  (reduce changed-at pois changes))

(defn- scanned [pois chunks [cx sy cz]]
  (let [x0 (* 16 (long cx)) y0 (* 16 (long sy)) z0 (* 16 (long cz))]
    (changed pois
             (for [z (range z0 (+ z0 16)) y (range y0 (+ y0 16))
                   x (range x0 (+ x0 16))
                   :let [p [x y z] st (chunk/at chunks p)]
                   :when (type-of st)]
               [p 0 st]))))

(defn loaded
  "Returns pois with the sections of chunk id of chunks that it
  lacks, each filled in the order a scan of it meets its points."
  [pois chunks id]
  (let [[cx cz] (chunk/id->pos id)
        c (chunk/chunk-at chunks cx cz)]
    (reduce (fn [pois si]
              (let [s (chunk/chunk-section c si)
                    sec [cx (- (long si) chunk/section-offset) cz]]
                (if (and s (not (contains? pois sec))
                         (chunk/holds? s @any))
                  (scanned pois chunks sec)
                  pois)))
            pois (range chunk/section-count))))

(defn listed
  "Returns the cells of type ty in section sec of pois in the order
  its hash set lists them."
  [pois sec ty]
  (let [{:keys [cells cap]} (get-in pois [sec ty])]
    (mapv cells (hash-order/at-capacity (map cell/hashed cells)
                                        (or cap 16)))))
