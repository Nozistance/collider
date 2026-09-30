(ns collider.world.feature.level
  "The level a tree reads and changes while it grows: the cells it
  sees, the writes it makes in order, and the draws of its random
  source."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(def horizontal
  "Direction.Plane.HORIZONTAL in its order."
  [:north :east :south :west])

(defn start
  "Returns a level over chunks. Its n-th draw is (roll [salt n])."
  [chunks roll salt]
  (let [n (volatile! -1)]
    {:chunks chunks :changes [] :trunks [] :foliage [] :roots []
     :decorations [] :leafy #{} :draw #(roll [salt (vswap! n inc)])}))

(defn draw ^double [lv] (double ((:draw lv))))

(defn next-int ^long [lv ^long n]
  (long (Math/floor (* (draw lv) n))))

(defn next-float ^double [lv] (double (float (draw lv))))

(defn next-bool [lv] (< (draw lv) 0.5))

(defn chance? [lv c] (< (next-float lv) (double (float c))))

(defn pick [lv xs] (nth xs (next-int lv (count xs))))

(defn at ^long [lv p] (chunk/at (:chunks lv) p))

(defn at-dir [p d] (mapv + p (dir/offset d)))

(defn toward
  "Returns p moved n cells in direction d."
  [p d n]
  (mapv #(+ (long %1) (* (long %2) (long n))) p (dir/offset d)))

(defn put
  "Returns lv after it sets st at p with flags, 19 as a tree does."
  ([lv p st] (put lv p st 19))
  ([lv p st flags]
   (-> lv
       (update :changes conj [p st nil flags])
       (update :chunks chunk/chunks-set-block p st))))

(defn placed
  "Returns lv after it sets st at p and notes p in the list k."
  [lv k p st]
  (cond-> (put (update lv k conj p) p st)
    (= :foliage k) (update :leafy conj p)))

(defn- tag-of [v] (if (map? v) (:tag v) (data/snake v)))

(defn in?
  "Returns true when st is in the holder set v: a tag or blocks."
  [st v]
  (let [st (long st)]
    (if (map? v)
      (block/tagged? st (:tag v))
      (boolean (some #{(block/block-of st)} v)))))

(defn air? [lv p] (block/air-type? (at lv p)))

(defn air-or-leaves? [lv p]
  (let [st (at lv p)]
    (or (block/air-type? st) (block/tagged? st "leaves"))))

(defn valid? [lv p]
  (let [st (at lv p)]
    (or (block/air-type? st) (block/tagged? st "replaceable_by_trees"))))

(defn free? [lv p]
  (or (valid? lv p) (block/tagged? (at lv p) "logs")))

(defn state-of ^long [m]
  (if-let [props (:props m)]
    (block/state (:block m) props)
    (block/state (:block m))))

(defn with-prop
  "Returns st with property k set to v."
  ^long [st k v]
  (let [st (long st)]
    (block/state (block/block-of st)
                 (assoc (block/props-of st) k
                        (if (keyword? v) v (keyword (str v)))))))

(defn weighted [lv entries]
  (let [total (reduce + (map :weight entries))]
    (loop [left (next-int lv total) [e & es] entries]
      (if (< left (long (:weight e)))
        (:data e)
        (recur (- left (long (:weight e))) es)))))

(defn sample
  "Returns a draw of the int provider v."
  ^long [lv v]
  (cond
    (number? v) (long v)
    (= :weighted-list (:type v)) (sample lv (weighted lv (:distribution v)))
    :else (let [lo (long (:min-inclusive v))]
            (+ lo (next-int lv (inc (- (long (:max-inclusive v)) lo)))))))

(defn- test? [lv pred p]
  (let [q (mapv + p (:offset pred [0 0 0]))]
    (case (:type pred)
      :not (not (test? lv (:predicate pred) p))
      :matching-block-tag (block/tagged? (at lv q) (tag-of (:tag pred)))
      :matching-blocks (in? (at lv q) (:blocks pred))
      :true true)))

(declare provide)

(defn optional
  "BlockStateProvider.getOptionalState: nil when no rule holds."
  [lv m p]
  (if (= :rule-based-state-provider (:type m))
    (if-let [r (first (filter #(test? lv (:if-true %) p) (:rules m)))]
      (provide lv (:then r) p)
      (when-let [f (:fallback m)] (provide lv f p)))
    (provide lv m p)))

(defn- randomized ^long [lv m p]
  (let [st (provide lv (:source m) p)
        k (:property m)]
    (if (contains? (block/props-of st) k)
      (with-prop st k (sample lv (:values m)))
      st)))

(defn provide
  "BlockStateProvider.getState of m at p."
  ^long [lv m p]
  (case (:type m)
    :simple-state-provider (state-of (:state m))
    :weighted-state-provider (state-of (weighted lv (:entries m)))
    :rule-based-state-provider (or (optional lv m p) (at lv p))
    :randomized-int-state-provider (randomized lv m p)))

(defn water? [lv p] (block/water? (at lv p)))

(defn source? [lv p]
  (let [st (at lv p)]
    (and (block/water? st)
         (or (not (block/liquid? st)) (zero? (block/liquid-level st))))))
