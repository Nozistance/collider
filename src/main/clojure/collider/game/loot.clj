(ns collider.game.loot
  "Mob loot tables."
  (:require [collider.data :as data]
            [collider.game.block.furnace :as furnace]
            [collider.game.stack :as stack]
            [collider.num :as num]))

(set! *warn-on-reflection* true)

(defn- fail [what data]
  (throw (ex-info (str "loot: " what) data)))

(defn- rounded ^long [x]
  (Math/round (double x)))

(defn- binomial ^long [n roll s]
  (let [p (double (:p n))]
    (count (filter #(< (double (roll (conj s %))) p)
                   (range (long (:n n)))))))

(defn- int-of ^long [n roll s]
  (cond
    (number? n) (rounded n)
    (:n n) (binomial n roll s)
    (:min n) (let [lo (rounded (:min n)) hi (rounded (:max n))]
               (if (>= lo hi)
                 lo
                 (+ lo (long (* (double (roll s)) (inc (- hi lo)))))))
    :else (fail "unknown number provider" {:provider n})))

(defn- float-of ^double [n roll s]
  (if (number? n)
    (double n)
    (let [lo (double (:min n)) hi (double (:max n))]
      (if (>= lo hi) lo (+ lo (* (double (roll s)) (- hi lo)))))))

(defn- members
  "Returns the entries of holder set v of registry, which is an id,
  a list of ids or a tag."
  [registry v]
  (cond
    (keyword? v) #{v}
    (:tag v) (set (data/tag-values registry (:tag v)))
    :else (set v)))

(defn- bounds [r]
  (if (number? r) {:min r :max r} r))

(defn- in-range? [r v]
  (let [r (bounds r)]
    (and (>= (long v) (long (:min r Long/MIN_VALUE)))
         (<= (long v) (long (:max r Long/MAX_VALUE))))))

(defn- enchanted?
  "Returns true when enchantment predicate p finds a match in the
  enchantment levels m."
  [p m]
  (let [r (:levels p)
        fits? #(and (pos? (long %)) (or (nil? r) (in-range? r %)))]
    (cond
      (:enchantments p)
      (boolean (some #(fits? (get m % 0))
                     (members "enchantment" (:enchantments p))))
      (:levels p) (boolean (some fits? (vals m)))
      :else (boolean (seq m)))))

(defn- item-part? [s [k v]]
  (case k
    (:enchantments :stored-enchantments)
    (let [m (stack/component s k)]
      (and (some? m) (every? #(enchanted? % m) v)))
    (fail "unknown item predicate" {:predicate k})))

(defn- item-matches? [p s]
  (every? (fn [[k v]]
            (case k
              :items (contains? (members "item" v) (:item s))
              :count (in-range? v (stack/size s))
              :predicates (every? #(item-part? s %) v)
              (fail "unknown item field" {:field k})))
          p))

(defn- equipped? [v e]
  (and (:living? e)
       (every? (fn [[slot p]]
                 (item-matches? p (get (:equipment e) slot)))
               v)))

(defn- flag? [k v e]
  (= (boolean v)
     (case k
       :is-on-fire (boolean (:on-fire? e))
       :is-baby (boolean (:baby? e))
       (fail "unknown entity flag" {:flag k}))))

(defn- same-flag? [a b]
  (= (boolean a) (boolean b)))

(defn- field? [e [k v]]
  (case k
    :flags (every? (fn [[fk fv]] (flag? fk fv e)) v)
    :components (= v (select-keys (:components e) (keys v)))
    :type-specific/sheep (same-flag? (:sheared v) (:sheared? e))
    :type-specific/raider (same-flag? (:is-captain v) (:captain? e))
    :type-specific/cube-mob (in-range? (:size v) (:size e 0))
    :vehicle (= (:entity-type v) (:vehicle e))
    :entity-type (contains? (members "entity_type" v) (:type e))
    :equipment (equipped? v e)
    (fail "unknown entity predicate" {:field k})))

(defn- entity-matches? [p e]
  (and (some? e) (every? #(field? e %) p)))

(defn- target [ctx t]
  (get ctx (if (= :this t) :entity t)))

(defn- properties? [c ctx]
  (let [p (:predicate c)]
    (or (empty? p) (entity-matches? p (target ctx (:entity c))))))

(defn- damage-tag? [t ctx]
  (let [vs (data/tag-values "damage_type" (data/snake (:id t)))]
    (= (boolean (:expected t))
       (boolean (some #{(:damage-type ctx)} vs)))))

(defn- damage? [p ctx]
  (every? (fn [[k v]]
            (case k
              :tags (every? #(damage-tag? % ctx) v)
              :source-entity (entity-matches? v (:attacker ctx))
              :direct-entity
              (entity-matches? v (:direct-attacker ctx))
              :is-direct (same-flag? v (:direct? ctx))
              (fail "unknown damage predicate" {:field k})))
          p))

(def ^:private slot-groups
  {:any [:mainhand :offhand :feet :legs :chest :head :body :saddle]
   :mainhand [:mainhand] :offhand [:offhand]
   :hand [:mainhand :offhand] :feet [:feet] :legs [:legs]
   :chest [:chest] :head [:head] :body [:body] :saddle [:saddle]
   :armor [:feet :legs :chest :head :body]})

(defn- slots-of [[id m]]
  [(data/kebab id) (mapv keyword (get m "slots"))])

(def ^:private ^:table enchant-slots
  (delay (into {} (map slots-of) (data/pack "enchantment"))))

(defn- level-on [e ench slot]
  (let [s (get (:equipment e) slot)]
    (long (get (stack/component s :enchantments) ench 0))))

(defn- level-of
  "Returns the highest level of enchantment ench on the slots it acts
  from, on the living attacker of ctx. Without one it is 0."
  ^long [ctx ench]
  (let [e (:attacker ctx)]
    (if (:living? e)
      (let [slots (mapcat slot-groups (get @enchant-slots ench))]
        (reduce max 0 (map #(level-on e ench %) slots)))
      0)))

(defn- level-value ^double [v ^long lvl]
  (cond
    (number? v) (num/f32 v)
    (= :linear (:type v))
    (let [per (num/f32 (:per-level-above-first v))]
      (num/f32 (+ (num/f32 (:base v)) (num/f32 (* per (dec lvl))))))
    :else (fail "unknown level value" {:value v})))

(defn- bonus-chance ^double [c ctx]
  (let [lvl (level-of ctx (:enchantment c))]
    (if (pos? lvl)
      (level-value (:enchanted-chance c) lvl)
      (num/f32 (:unenchanted-chance c)))))

(declare outcomes)

(defn- passes? [c ctx roll s]
  (case (:condition c)
    :killed-by-player (some? (:attacking-player ctx))
    :random-chance (< (double (roll s)) (double (:chance c)))
    :random-chance-with-enchanted-bonus
    (< (double (roll s)) (bonus-chance c ctx))
    :entity-properties (properties? c ctx)
    :damage-source-properties (damage? (:predicate c) ctx)
    :inverted (not (passes? (:term c) ctx roll (conj s :term)))
    :any-of (boolean (some true? (outcomes (:terms c) ctx roll s)))
    :all-of (every? true? (outcomes (:terms c) ctx roll s))
    (fail "unknown condition" {:condition (:condition c)})))

(defn- outcomes [cs ctx roll s]
  (map-indexed #(passes? %2 ctx roll (conj s %1)) cs))

(defn- met? [cs ctx roll s]
  (every? true? (outcomes cs ctx roll (conj s :when))))

(defn- smelt [stack]
  (if-let [r (:out (furnace/recipe :furnace stack))]
    (let [n (* (long (:count stack 1)) (long (:count r 1)))]
      (assoc r :count (min n (data/max-stack (:item r)))))
    stack))

(defn- count-increase [stack f ctx roll s]
  (let [lvl (level-of ctx (:enchantment f))
        lim (long (:limit f 0))]
    (if (zero? lvl)
      stack
      (let [add (num/f32 (* lvl (float-of (:count f) roll s)))
            n (+ (long (:count stack 1)) (Math/round (float add)))]
        (assoc stack :count (if (pos? lim) (min n lim) n))))))

(defn- set-count [stack f roll s]
  (let [base (if (:add f) (long (:count stack 1)) 0)
        n (int-of (:count f) roll (conj s :n))]
    (assoc stack :count (+ base n))))

(defn- ominous-amplifier [stack f roll s]
  (let [n (int-of (:amplifier f) roll (conj s :a))]
    (assoc-in stack [:components :ominous-bottle-amplifier]
              (min 4 (max 0 n)))))

(defn- run-fn [stack f ctx roll s]
  (case (:function f)
    :set-count (set-count stack f roll s)
    :enchanted-count-increase
    (count-increase stack f ctx roll (conj s :l))
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

(defn- expand [e ctx roll s]
  (if-not (met? (:conditions e) ctx roll s)
    []
    (case (:type e)
      (:item :loot-table :empty) [[e s]]
      :alternatives (let [kids (children-of e ctx roll s)]
                      (or (first (remove empty? kids)) []))
      :group (vec (mapcat identity (children-of e ctx roll s)))
      :sequence (till-empty (children-of e ctx roll s))
      (fail "unsupported entry" {:type (:type e)}))))

(defn- weight-of
  "Returns the weight of leaf e under luck, never below 0."
  ^long [[e _] ^double luck]
  (let [q (num/f32 (* (num/f32 (:quality e 0)) luck))]
    (max 0 (long (Math/floor (num/f32 (+ (long (:weight e 1)) q)))))))

(defn- pick [leaves ^double r ^double luck]
  (let [w #(weight-of % luck)
        ls (filterv #(pos? (long (w %))) leaves)
        total (reduce + 0 (map w ls))]
    (when (pos? (long total))
      (loop [[l & more] ls i (long (* r (long total)))]
        (if (neg? (- i (long (w l))))
          l
          (recur more (- i (long (w l)))))))))

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
    (if-let [l (pick (vec leaves) (double (roll (conj s :pick)))
                     (double (:luck ctx 0.0)))]
      (mapv #(apply-fns % (:functions p) ctx roll (conj s :fns))
            (leaf-stacks tables l ctx roll))
      [])))

(defn- bonus-rolls
  "Returns the rolls that the luck of ctx adds to pool p."
  ^long [p ctx roll s]
  (if-let [b (:bonus-rolls p)]
    (let [luck (num/f32 (:luck ctx 0.0))
          n (num/f32 (* (num/f32 (float-of b roll s)) luck))]
      (long (Math/floor n)))
    0))

(defn- pool-drops [tables p ctx roll s]
  (if-not (met? (:conditions p) ctx roll s)
    []
    (let [n (+ (int-of (:rolls p 1) roll (conj s :rolls))
               (bonus-rolls p ctx roll (conj s :bonus)))]
      (into [] (mapcat #(one-roll tables p ctx roll (conj s %)))
            (range n)))))

(defn- table-drops [tables id ctx roll s]
  (let [t (or (get tables id) (fail "unknown table" {:table id}))]
    (into [] (comp (map-indexed
                    #(pool-drops tables %2 ctx roll (conj s %1)))
                   cat)
          (:pools t))))

(defn drops
  "Returns the stacks table-id drops in ctx, in pool order.
  The roll function gives a number from 0 to 1 for a salt. Every
  decision draws its own salt."
  [tables table-id ctx roll]
  (filterv #(pos? (long (:count % 1)))
           (table-drops tables table-id ctx roll [table-id])))
