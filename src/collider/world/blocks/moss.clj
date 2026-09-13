(ns collider.world.blocks.moss
  "Pale moss carpet and hanging moss."
  (:require [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def wall-sides [:north :east :south :west])
(defn- with ^long [^long st k v]
  (block/state (block/block-of st) (assoc (block/props-of st) k v)))

(defn- attachable? [chunks p dir]
  (let [n (gen/at-void chunks (mapv + p (dir/offset dir)))]
    (and (pos? n) (block/face-sturdy? n (dir/opposite dir)))))

(defn- carpet-at? [chunks p dir pred]
  (let [n (gen/at-void chunks p)]
    (and (= :pale-moss-carpet (block/block-of n)) (pred (block/props-of n) dir))))

(defn- carpet-side [chunks p st dir create?]
  (let [st (long st) props (block/props-of st)
        side (if (attachable? chunks p dir) (if create? :low (get props dir)) :none)]
    (cond
      (not= :low side) side
      (carpet-at? chunks (dir/up p) dir (fn [m d] (and (not= :none (get m d)) (= :false (:bottom m))))) :tall
      (and (= :false (:bottom props)) (carpet-at? chunks (dir/down p) dir (fn [m d] (= :none (get m d))))) :none
      :else :low)))

(defn carpet-updated
  "Returns the carpet st at p with each side set from what stands beside it.
   create-sides? grows sides it does not yet have."
  ^long [chunks p ^long st create-sides?]
  (let [create? (or create-sides? (= :true (:bottom (block/props-of st))))]
    (reduce (fn [s dir] (with s dir (carpet-side chunks p s dir create?))) st wall-sides)))

(defn carpet-faces?
  "Returns true when the carpet st has a bottom or at least one side."
  [^long st]
  (let [props (block/props-of st)]
    (or (= :true (:bottom props)) (boolean (some #(not= :none (get props %)) wall-sides)))))

(defn carpet-supported?
  "Returns true when what stands below p holds the carpet st."
  [chunks p ^long st]
  (let [below (gen/at-void chunks (dir/down p))]
    (if (= :true (:bottom (block/props-of st)))
      (pos? below)
      (and (= :pale-moss-carpet (block/block-of below)) (= :true (:bottom (block/props-of below)))))))

(defn carpet-reshaped
  "Returns the state the carpet st at p becomes now, air when nothing holds it."
  ^long [chunks p ^long st]
  (if-not (carpet-supported? chunks p st)
    0
    (let [st' (carpet-updated chunks p st false)]
      (if (carpet-faces? st') st' 0))))

(defn carpet-topper
  "Returns the carpet state to place above p, or nil when none fits there. side?
   tells which sides it may keep."
  [chunks p side?]
  (let [above (dir/up p) prev (gen/at-void chunks above)
        carpet? (= :pale-moss-carpet (block/block-of prev))]
    (when (and (chunk/in-range? (above 1))
               (or (not carpet?) (= :false (:bottom (block/props-of prev))))
               (or carpet? (block/can-be-replaced? prev)))
      (let [base (carpet-updated chunks above (block/state :pale-moss-carpet {:bottom :false}) true)
            st' (reduce (fn [s dir]
                          (if (and (not= :none (get (block/props-of s) dir)) (not (side? dir))) (with s dir :none) s))
                        base wall-sides)]
        (when (and (carpet-faces? st') (not= st' prev)) st')))))

(defn hanging-tip
  "Returns st marked as a tip or not, from whether more of the same moss hangs
   below p."
  ^long [chunks p ^long st]
  (with st :tip (if (= (block/block-of st) (block/block-of (gen/at-void chunks (dir/down p)))) :false :true)))

(defn hanging-supported?
  "Returns true when what stands above p holds the hanging moss st."
  [chunks p ^long st]
  (let [above (gen/at-void chunks (dir/up p))]
    (or (attachable? chunks p :up) (= (block/block-of st) (block/block-of above)))))

(defn hanging-end
  "Returns the position just past the bottom of the column of self that starts
   at p."
  [chunks p self]
  (loop [q (dir/down p)]
    (if (= self (block/block-of (gen/at-void chunks q))) (recur (dir/down q)) q)))
