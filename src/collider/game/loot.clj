(ns collider.game.loot
  "Rolling the vanilla loot tables of the mobs."
  (:require [collider.data :as data]
            [collider.game.block.furnace :as furnace]))

(set! *warn-on-reflection* true)

(defn- fail [what data]
  (throw (ex-info (str "loot: " what) data)))

(defn- rounded ^long [x]
  (Math/round (double x)))

(defn- binomial ^long [n roll s]
  (let [p (double (:p n))]
    (count (filter #(< (double (roll (conj s %))) p)
                   (range (long (:n n)))))))

(defn- int-count ^long [n roll s]
  (cond
    (number? n) (rounded n)
    (:n n) (binomial n roll s)
    (:min n) (let [lo (rounded (:min n)) hi (rounded (:max n))]
               (if (>= lo hi)
                 lo
                 (+ lo (long (* (double (roll s)) (inc (- hi lo)))))))
    :else (fail "unknown number provider" {:provider n})))

(defn- float-count ^double [n roll s]
  (if (number? n)
    (double n)
    (let [lo (double (:min n)) hi (double (:max n))]
      (if (>= lo hi) lo (+ lo (* (double (roll s)) (- hi lo)))))))

(defn- flag? [k v ctx]
  (= (boolean v)
     (case k
       :is-on-fire (boolean (:on-fire? ctx))
       :is-baby (boolean (:baby? (:entity ctx)))
       (fail "unknown entity flag" {:flag k}))))

(defn- at-least? [r v]
  (and (>= (long v) (long (:min r Long/MIN_VALUE)))
       (<= (long v) (long (:max r Long/MAX_VALUE)))))

(defn- same-flag? [a b]
  (= (boolean a) (boolean b)))

(defn- field? [k v ctx]
  (let [e (:entity ctx)]
    (case k
      :flags (every? (fn [[fk fv]] (flag? fk fv ctx)) v)
      :components (= v (select-keys (:components e) (keys v)))
      :type-specific/sheep (same-flag? (:sheared v) (:sheared? e))
      :type-specific/raider (same-flag? (:is-captain v) (:captain? e))
      :type-specific/cube-mob (at-least? (:size v) (:size e 0))
      :vehicle (= (:entity-type v) (:vehicle e))
      :entity-type (= v (:type e))
      :equipment false
      (fail "unknown entity predicate" {:field k}))))

(defn- this? [c ctx]
  (and (= :this (:entity c))
       (every? (fn [[k v]] (field? k v ctx)) (:predicate c))))

(defn- damage-tag? [t ctx]
  (let [vs (data/tag-values "damage_type" (data/snake (:id t)))]
    (= (boolean (:expected t))
       (boolean (some #{(:damage-type ctx)} vs)))))

(defn- damage? [p ctx]
  (and (every? #{:tags} (keys p))
       (every? #(damage-tag? % ctx) (:tags p))))

(defn- bonus-chance ^double [c ctx]
  (let [lvl (long (:looting ctx 0))
        e (:enchanted-chance c)]
    (when-not (= :linear (:type e))
      (fail "unknown enchanted chance" {:chance e}))
    (if (pos? lvl)
      (+ (double (:base e))
         (* (double (:per-level-above-first e)) (dec lvl)))
      (double (:unenchanted-chance c)))))

(declare all?)

(defn- passes? [c ctx roll s]
  (case (:condition c)
    :killed-by-player (boolean (:killed-by-player? ctx))
    :random-chance (< (double (roll s)) (double (:chance c)))
    :random-chance-with-enchanted-bonus
    (< (double (roll s)) (bonus-chance c ctx))
    :entity-properties (this? c ctx)
    :damage-source-properties (damage? (:predicate c) ctx)
    :inverted (not (passes? (:term c) ctx roll (conj s :term)))
    :any-of (boolean (some true? (all? (:terms c) ctx roll s)))
    :all-of (every? true? (all? (:terms c) ctx roll s))
    (fail "unknown condition" {:condition (:condition c)})))

(defn- all?
  "The verdicts of cs, one distinct salt each."
  [cs ctx roll s]
  (map-indexed #(passes? %2 ctx roll (conj s %1)) cs))

(defn- met? [cs ctx roll s]
  (every? true? (all? cs ctx roll (conj s :when))))

(defn- smelt [stack]
  (if-let [r (:out (furnace/recipe :furnace stack))]
    (let [n (* (long (:count stack 1)) (long (:count r 1)))]
      (assoc r :count (min n (data/max-stack (:item r)))))
    stack))

(defn- looting-grow [stack f ctx roll s]
  (let [lvl (long (:looting ctx 0))
        lim (long (:limit f 0))]
    (if (zero? lvl)
      stack
      (let [n (+ (long (:count stack 1))
                 (rounded (* lvl (float-count (:count f) roll s))))]
        (assoc stack :count (if (pos? lim) (min n lim) n))))))

(defn- set-count [stack f roll s]
  (let [base (if (:add f) (long (:count stack 1)) 0)
        n (int-count (:count f) roll (conj s :n))]
    (assoc stack :count (+ base n))))

(defn- ominous-amplifier [stack f roll s]
  (let [n (int-count (:amplifier f) roll (conj s :a))]
    (assoc-in stack [:components :ominous-bottle-amplifier]
              (min 4 (max 0 n)))))

(defn- run-fn [stack f ctx roll s]
  (case (:function f)
    :set-count (set-count stack f roll s)
    :enchanted-count-increase
    (looting-grow stack f ctx roll (conj s :l))
    :furnace-smelt (smelt stack)
    :set-potion (assoc-in stack [:components :potion-contents]
                {:potion (:id f)})
    :set-ominous-bottle-amplifier (ominous-amplifier stack f roll s)
    (fail "unsupported function" {:function (:function f)})))

(defn- apply-fn [stack f ctx roll s]
  (if (met? (:conditions f) ctx roll s)
    (run-fn stack f ctx roll s)
    stack))

(defn- apply-fns [stack fs ctx roll s]
  (reduce-kv #(apply-fn %1 %3 ctx roll (conj s %2)) stack (vec fs)))

(declare expand)

(defn- children-of [e ctx roll s]
  (map-indexed #(expand %2 ctx roll (conj s %1)) (:children e)))

(defn- till-empty [xs]
  (vec (mapcat identity (take-while seq xs))))

(defn- expand
  "The singleton entries e yields, each with its own salt."
  [e ctx roll s]
  (if-not (met? (:conditions e) ctx roll s)
    []
    (case (:type e)
      (:item :loot-table :empty) [[e s]]
      :alternatives (let [kids (children-of e ctx roll s)]
                      (or (first (remove empty? kids)) []))
      :group (vec (mapcat identity (children-of e ctx roll s)))
      :sequence (till-empty (children-of e ctx roll s))
      (fail "unsupported entry" {:type (:type e)}))))

(defn- weight-of ^long [[e _]]
  (long (:weight e 1)))

(defn- pick [leaves ^double r]
  (let [ls (filterv #(pos? (weight-of %)) leaves)
        total (reduce + 0 (map weight-of ls))]
    (when (pos? (long total))
      (loop [[l & more] ls i (long (* r (long total)))]
        (if (neg? (- i (weight-of l)))
          l
          (recur more (- i (weight-of l))))))))

(declare table-drops)

(defn- nested [tables e ctx roll s]
  (let [id (:value e)]
    (table-drops tables id ctx roll (conj s id))))

(defn- leaf-stacks [tables [e s] ctx roll]
  (let [made (case (:type e)
               :empty []
               :item [{:item (:name e) :count 1}]
               :loot-table (nested tables e ctx roll s))]
    (mapv #(apply-fns % (:functions e) ctx roll (conj s :fns)) made)))

(defn- one-roll [tables p ctx roll s]
  (let [expand-at (fn [i e] (expand e ctx roll (conj s i)))
        leaves (mapcat identity (map-indexed expand-at (:entries p)))]
    (if-let [l (pick (vec leaves) (double (roll (conj s :pick))))]
      (mapv #(apply-fns % (:functions p) ctx roll (conj s :fns))
            (leaf-stacks tables l ctx roll))
      [])))

(defn- pool-drops [tables p ctx roll s]
  (if-not (met? (:conditions p) ctx roll s)
    []
    (let [n (int-count (:rolls p 1) roll (conj s :rolls))]
      (into [] (mapcat #(one-roll tables p ctx roll (conj s %)))
            (range n)))))

(defn- table-drops [tables id ctx roll s]
  (let [t (or (get tables id) (fail "unknown table" {:table id}))]
    (into [] (comp (map-indexed
                    #(pool-drops tables %2 ctx roll (conj s %1)))
                   cat)
          (:pools t))))

(defn drops
  "The stacks table-id drops in ctx, in pool order.

  ctx is {:on-fire? :killed-by-player? :damage-type :looting :entity},
  where :entity is the dying mob: :type, :baby?, :sheared?, :captain?,
  :size, :vehicle (the type ridden) and :components, the map the
  predicates read, keyed as in the data: {:sheep/color :black}.
  roll is (roll salt) -> [0,1); every decision gets its own salt.
  Luck is zero, so bonus rolls and quality do not count; a
  predicate on the killer or on equipment never holds yet."
  [tables table-id ctx roll]
  (filterv #(pos? (long (:count % 1)))
           (table-drops tables table-id ctx roll [table-id])))
