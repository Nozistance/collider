(ns collider.world.blocks.moss
  "Pale moss carpet and hanging moss."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.multiface :as multiface]))

(set! *warn-on-reflection* true)

(defn- carpet-at? [chunks p dir pred]
  (let [n (chunk/at-void chunks p)]
    (and (= :pale-moss-carpet (block/block-of n))
         (pred (block/props-of n) dir))))

(defn- tall-above? [m d]
  (and (not= :none (get m d)) (= :false (:bottom m))))

(defn- bare-below? [m d]
  (= :none (get m d)))

(defn- carpet-side [chunks p st dir create?]
  (let [st (long st) props (block/props-of st)
        side (if (multiface/attaches? chunks p dir)
               (if create? :low (get props dir))
               :none)]
    (cond
      (not= :low side) side
      (carpet-at? chunks (dir/up p) dir tall-above?) :tall
      (and (= :false (:bottom props))
           (carpet-at? chunks (dir/down p) dir bare-below?))
      :none
      :else :low)))

(defn carpet-updated
  "Returns the carpet st at p with its sides set from what stands
  beside it. With create-sides? it also adds sides it lacks."
  ^long [chunks p ^long st create-sides?]
  (let [bottom? (= :true (:bottom (block/props-of st)))
        create? (or create-sides? bottom?)]
    (reduce (fn [s dir]
              (block/with s dir (carpet-side chunks p s dir create?)))
            st dir/horizontal)))

(defn carpet-faces?
  "Returns true when the carpet st shows a bottom or a side."
  [^long st]
  (let [props (block/props-of st)]
    (or (= :true (:bottom props))
        (boolean (some #(not= :none (get props %)) dir/horizontal)))))

(defn carpet-supported?
  "Returns true when the block below holds the carpet st at p."
  [chunks p ^long st]
  (let [below (chunk/at-void chunks (dir/down p))]
    (if (= :true (:bottom (block/props-of st)))
      (pos? below)
      (and (= :pale-moss-carpet (block/block-of below))
           (= :true (:bottom (block/props-of below)))))))

(defn carpet-reshaped
  "Returns the carpet st at p fitted to its neighbours, or air when
  it has no hold or no face left."
  ^long [chunks p ^long st]
  (if-not (carpet-supported? chunks p st)
    0
    (let [st' (carpet-updated chunks p st false)]
      (if (carpet-faces? st') st' 0))))

(defn- trimmed [s dir side?]
  (if (and (not= :none (get (block/props-of s) dir))
           (not (side? dir)))
    (block/with s dir :none)
    s))

(defn carpet-topper
  "Returns the carpet state to place above p, or nil when none
  fits. side? tells which sides it may keep."
  [chunks p side?]
  (let [above (dir/up p) prev (chunk/at-void chunks above)
        carpet? (= :pale-moss-carpet (block/block-of prev))
        open? (and carpet? (= :false (:bottom (block/props-of prev))))
        fresh (block/state :pale-moss-carpet {:bottom :false})]
    (when (and (chunk/in-range? (above 1))
               (or (not carpet?) open?)
               (or carpet? (block/can-be-replaced? prev)))
      (let [base (carpet-updated chunks above fresh true)
            st' (reduce #(trimmed %1 %2 side?) base dir/horizontal)]
        (when (and (carpet-faces? st') (not= st' prev)) st')))))

(defn hanging-tip
  "Returns the hanging moss st at p marked as a tip unless more moss
  hangs below it."
  ^long [chunks p ^long st]
  (let [below (chunk/at-void chunks (dir/down p))
        same? (= (block/block-of st) (block/block-of below))]
    (block/with st :tip (block/flag (not same?)))))

(defn hanging-supported?
  "Returns true when the block above holds the hanging moss st at p."
  [chunks p ^long st]
  (let [above (chunk/at-void chunks (dir/up p))]
    (or (multiface/attaches? chunks p :up)
        (= (block/block-of st) (block/block-of above)))))

(defn hanging-end
  "Returns the first cell below p that is not hanging moss of self."
  [chunks p self]
  (loop [q (dir/down p)]
    (if (= self (block/block-of (chunk/at-void chunks q)))
      (recur (dir/down q))
      q)))
