(ns collider.world.blocks.dust
  "The power of redstone wire and what a wire tells when it changes."
  (:require [collider.cell :as cell]
            [collider.hash-order :as hash-order]
            [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.blocks.wire :as wire]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private horizontal [:north :east :south :west])

(defn- power-of ^long [^long st]
  (if (wire/wire? st) (block/prop-long st :power) 0))

(defn- incoming
  "Returns the power the wires beside p give it, one less than the
  strongest, reaching a step up past a conductor and a step down past
  anything else."
  ^long [chunks p]
  (let [at #(chunk/at chunks %)
        up-open? (not (block/conductor? (at (dir/toward p :up))))
        best (reduce
               (fn [^long m d]
                 (let [q (dir/toward p d)
                       n (at q)
                       m (max m (power-of n))]
                   (cond
                     (block/conductor? n)
                     (if up-open? (max m (power-of (at (dir/toward q :up)))) m)
                     :else (max m (power-of (at (dir/toward q :down)))))))
               0 horizontal)]
    (max 0 (dec best))))

(defn- target ^long [chunks p]
  (let [s (signal/best-neighbor-signal chunks p false)]
    (if (= 15 s) s (max s (incoming chunks p)))))

(defn- told-cells
  "Returns p and the cells around it in the order a hash set of them
  gives them back."
  [p]
  (let [cells (into [p] (map #(dir/toward p %)) dir/six)]
    (mapv cells (hash-order/of (map cell/hashed cells)))))

(defn powered
  "Returns the changes of wire st at p working out its power. The new
  power is set only while the block at p is still st, and p and the
  cells around it are told of it."
  [chunks p ^long st]
  (let [t (target chunks p)]
    (when (not= t (power-of st))
      (cond-> []
        (= st (chunk/at chunks p))
        (conj [p (block/with-long st :power t) nil update/clients])
        true (into (map (fn [q] [:notify q nil st])) (told-cells p))))))

(defn- corner
  "Returns the step that tells the cells at and around q when q holds
  wire."
  [q]
  (fn [chunks]
    (let [st (chunk/at chunks q)]
      (when (wire/wire? st)
        (into [[:notify q nil st]]
              (map (fn [d] [:notify (dir/toward q d) nil st]))
              dir/six)))))

(defn- corner-beside [p d]
  (fn [chunks]
    (let [q (dir/toward p d)
          v (if (block/conductor? (chunk/at chunks q)) :up :down)]
      ((corner (dir/toward q v)) chunks))))

(defn wires-told
  "Returns the steps that tell the wires beside p and a step up or
  down from them."
  [p]
  (into (mapv #(corner (dir/toward p %)) horizontal)
        (map #(corner-beside p %))
        horizontal))

(defn removed
  "Returns the ops and steps of wire st that left p."
  [p ^long st]
  (-> (mapv #(vector :notify (dir/toward p %) nil st) dir/six)
      (conj (fn [chunks] (powered chunks p st)))
      (into (wires-told p))))

(defn- placed [chunks p {:keys [old]}]
  (let [st (chunk/at chunks p)]
    (when-not (wire/wire? (long old))
      (-> (vec (powered chunks p st))
          (conj [:notify (dir/toward p :up) nil st]
                [:notify (dir/toward p :down) nil st])
          (into (wires-told p))))))

(defn- changed [chunks p {:keys [given] :as ctx}]
  (if (support/supported? chunks p (chunk/at chunks p))
    (powered chunks p (long given))
    ((:reshape support/rule) chunks p ctx)))

(def rule
  "The block rule of redstone wire. It is held as attached blocks are
  and works out its power as its neighbours change."
  (assoc support/rule
         :name :dust
         :match? (fn [_chunks st _p] (wire/wire? st))
         :pass :both
         :wake (constantly :neighbor)
         :reshape (fn [chunks p ctx]
                    (case (:pass ctx)
                      :place (placed chunks p ctx)
                      :neighbor (changed chunks p ctx)
                      ((:reshape support/rule) chunks p ctx)))))
