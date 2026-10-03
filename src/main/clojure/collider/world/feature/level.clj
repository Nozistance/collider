(ns collider.world.feature.level
  "The level a tree grows in, with the writes it makes in order and
  the draws of its random source."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tree-flags
  (bit-or update/all update/known-shape))

(def horizontal
  "The horizontal directions in the order a tree draws them."
  [:north :east :south :west])

(defn start
  "Returns a level over chunks. Its n-th draw is (roll [salt n])."
  [chunks roll salt]
  (let [n (volatile! -1)]
    {:chunks chunks :changes [] :trunks [] :foliage [] :roots []
     :decorations [] :leafy #{} :draw #(roll [salt (vswap! n inc)])}))

(defn draw
  "Returns the next draw of lv, from 0 below 1."
  ^double [lv]
  (double ((:draw lv))))

(defn next-int
  "Returns a whole number from 0 below n by the next draw of lv."
  ^long [lv ^long n]
  (random/below (draw lv) n))

(defn next-float
  "Returns the next draw of lv rounded to a float."
  ^double [lv]
  (double (float (draw lv))))

(defn next-bool
  "Returns true for half of the draws of lv."
  [lv]
  (< (draw lv) 0.5))

(defn chance?
  "Returns true with chance c by the next draw of lv."
  [lv c]
  (< (next-float lv) (double (float c))))

(defn pick
  [lv xs]
  (nth xs (next-int lv (count xs))))

(defn at
  ^long [lv p]
  (chunk/at (:chunks lv) p))

(defn off
  [p dx dy dz]
  [(+ (long (p 0)) (long dx)) (+ (long (p 1)) (long dy))
   (+ (long (p 2)) (long dz))])

(defn at-dir
  [p d]
  (mapv + p (dir/offset d)))

(defn toward
  [p d n]
  (mapv #(+ (long %1) (* (long %2) (long n))) p (dir/offset d)))

(defn manhattan
  ^long [a b]
  (reduce + (map #(Math/abs (- (long %1) (long %2))) a b)))

(defn put
  "Returns lv after it sets st at p with flags, the flags of a tree
  when none are given."
  ([lv p st] (put lv p st tree-flags))
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
  "Returns true when st is in the holder set v, a tag or blocks."
  [st v]
  (let [st (long st)]
    (if (map? v)
      (block/tagged? st (:tag v))
      (boolean (some #{(block/block-of st)} v)))))

(defn air?
  [lv p]
  (block/air-type? (at lv p)))

(defn air-or-leaves?
  [lv p]
  (let [st (at lv p)]
    (or (block/air-type? st) (block/tagged? st "leaves"))))

(defn valid?
  "Returns true when a tree may take the cell p of lv."
  [lv p]
  (let [st (at lv p)]
    (or (block/air-type? st)
        (block/tagged? st "replaceable_by_trees"))))

(defn state-of
  "Returns the block state that the state value m names."
  ^long [m]
  (if-let [props (:props m)]
    (block/state (:block m) props)
    (block/state (:block m))))

(defn weighted
  "Returns the data of an entry of entries, drawn by weight."
  [lv entries]
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
    (= :weighted-list (:type v))
    (sample lv (weighted lv (:distribution v)))
    :else (let [r (draw lv)
                lo (long (:min-inclusive v))]
            (random/between r lo (long (:max-inclusive v))))))

(defn- test? [lv pred p]
  (let [q (mapv + p (:offset pred [0 0 0]))]
    (case (:type pred)
      :not (not (test? lv (:predicate pred) p))
      :matching-block-tag
      (block/tagged? (at lv q) (tag-of (:tag pred)))
      :matching-blocks (in? (at lv q) (:blocks pred))
      :true true)))

(declare provide)

(defn- ruled [lv m p]
  (if-let [r (first (filter #(test? lv (:if-true %) p) (:rules m)))]
    (provide lv (:then r) p)
    (when-let [f (:fallback m)] (provide lv f p))))

(defn optional
  "Returns the state that provider m gives at p, nil when none of
  its rules holds."
  [lv m p]
  (if (= :rule-based-state-provider (:type m))
    (ruled lv m p)
    (provide lv m p)))

(defn- randomized ^long [lv m p]
  (let [st (provide lv (:source m) p)
        k (:property m)]
    (if (contains? (block/props-of st) k)
      (block/with st k (sample lv (:values m)))
      st)))

(defn provide
  ^long [lv m p]
  (case (:type m)
    :simple-state-provider (state-of (:state m))
    :weighted-state-provider (state-of (weighted lv (:entries m)))
    :rule-based-state-provider (or (ruled lv m p) (at lv p))
    :randomized-int-state-provider (randomized lv m p)))

(defn water?
  [lv p]
  (block/water? (at lv p)))

(defn source?
  [lv p]
  (let [st (at lv p)]
    (and (block/water? st)
         (or (not (block/liquid? st))
             (zero? (block/liquid-level st))))))
