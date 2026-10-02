(ns collider.world.blocks.halves
  "Blocks made of two halves, and the way from one half to the other."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.chest :as chest]))

(set! *warn-on-reflection* true)

(def pair-types
  "The plant types that stand two blocks tall."
  #{:double-plant :tall-flower :tall-seagrass :small-dripleaf})

(def half-types
  "The block types with a lower and an upper half."
  (into block/door-types (conj pair-types :pitcher-crop)))

(defn- bed-offset [{:keys [part facing]}]
  (dir/horizontal-offset
    (if (= :foot part) facing (dir/opposite facing))))

(defn partner-offset
  "Returns the offset to the other half of the two-block st, or nil."
  [^long st]
  (let [{:keys [half] :as props} (block/props-of st)
        t (block/type-of st)]
    (cond
      (contains? half-types t) (if (= :lower half) [0 1 0] [0 -1 0])
      (= :bed t) (bed-offset props)
      (contains? chest/types t)
      (dir/offset (chest/connected-direction st)))))

(defn- paired? [^long st ^long other]
  (if (contains? chest/types (block/type-of st))
    (chest/paired? st other)
    (and (= (block/block-of st) (block/block-of other))
         (let [k (if (= :bed (block/type-of st)) :part :half)]
           (not= (k (block/props-of st))
                 (k (block/props-of other)))))))

(defn partner
  "Returns [pos state] of the other half of st at pos, or nil."
  [chunks pos ^long st]
  (when-let [off (partner-offset st)]
    (let [p (mapv + pos off) o (chunk/at chunks p)]
      (when (paired? st o) [p o]))))
