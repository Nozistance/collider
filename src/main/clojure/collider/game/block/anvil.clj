(ns collider.game.block.anvil
  "Anvil results, costs and names."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.block.menu :as menu]
            [collider.game.changes :as changes]
            [collider.game.enchantment :as enchantment]
            [collider.game.out :as out]
            [collider.game.stack :as stack]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]))

(set! *warn-on-reflection* true)

(def ^:const too-expensive 40)

(def ^:private ^:const rename-cost 1)

(def ^:private ^:const sacrifice-cost 2)

(defn- of [name] (enchantment/info name))

(defn- max-level ^long [name] (long (:max-level (of name) 1)))

(defn- anvil-cost ^long [name] (long (:anvil-cost (of name) 0)))

(defn- supported? [name stack]
  (contains? (:supported (of name)) (:item stack)))

(defn- rivals? [a b]
  (and (not= a b)
       (or (contains? (:exclusive (of a)) b)
           (contains? (:exclusive (of b)) a))))

(defn- repairs? [input addition]
  (and (stack/damageable? input)
       (contains? (data/repairable (:item input))
                  (:item addition))))

(defn- unit-repair ^long [result]
  (min (stack/damage result)
       (quot (stack/max-damage result) 4)))

(defn- mend [result ^long units]
  (loop [r result n 0]
    (let [by (unit-repair r)]
      (if (or (zero? by) (>= n units))
        [r n]
        (recur (stack/with-damage r (- (stack/damage r) by))
               (inc n))))))

(defn- price-of ^long [m] (long (:price m)))

(defn- material-repair [m result addition]
  (if (zero? (unit-repair result))
    (assoc m :result nil :price 0 :stop true)
    (let [[r n] (mend result (stack/size addition))]
      (assoc m :result r :price (+ (price-of m) n)
               :repair-count n))))

(defn- left-uses ^long [s]
  (- (stack/max-damage s) (stack/damage s)))

(defn- sacrifice [m result input addition]
  (let [left (+ (left-uses input) (left-uses addition)
                (quot (* (stack/max-damage result) 12) 100))
        dmg (max 0 (- (stack/max-damage result) left))]
    (if (< dmg (stack/damage result))
      (assoc m :result (stack/with-damage result dmg)
               :price (+ (price-of m) sacrifice-cost))
      (assoc m :result result))))

(defn- fee ^long [name ^long level book?]
  (let [c (anvil-cost name)]
    (* level (if book? (max 1 (quot c 2)) c))))

(defn- levelled ^long [^long have ^long want]
  (if (= have want) (inc want) (max want have)))

(defn- conflicts ^long [name have]
  (count (filter #(rivals? name %) (keys have))))

(defn- allowed? [m input name ^long bad]
  (and (zero? bad)
       (or (:creative? m)
           (= :enchanted-book (:item input))
           (supported? name input))))

(defn- merged-one [m input book? [name level]]
  (let [have (:enchantments m)
        lvl (levelled (long (get have name 0)) (long level))
        bad (conflicts name have)
        lvl (min lvl (max-level name) 255)
        price (+ (price-of m) bad)]
    (if-not (allowed? m input name bad)
      (assoc m :price price :refused true)
      (let [paid (+ price (fee name lvl book?))
            paid (if (> (stack/size input) 1) too-expensive paid)]
        (assoc m :took true :price paid
                 :enchantments (assoc have name lvl))))))

(defn- merge-enchantments [m input addition book?]
  (let [adds (stack/enchantments addition)
        pairs (map (fn [k] [k (get adds k)]) (sort (keys adds)))
        m' (reduce #(merged-one %1 input book? %2) m pairs)]
    (if (and (:refused m') (not (:took m')))
      (assoc m' :result nil :price 0 :stop true)
      m')))

(defn- mismatched? [result addition book?]
  (and (not book?)
       (or (not= (:item result) (:item addition))
           (not (stack/damageable? result)))))

(defn- combined [m input addition]
  (let [book? (stack/has? addition :stored-enchantments)
        result (:result m)]
    (cond
      (repairs? result addition)
      (material-repair m result addition)
      (mismatched? result addition book?)
      (assoc m :result nil :price 0 :stop true)
      :else
      (-> (if (and (stack/damageable? result) (not book?))
            (sacrifice m result input addition)
            m)
          (merge-enchantments input addition book?)))))

(defn- named [m result name]
  (assoc m :result (stack/put result :custom-name name)
           :price (+ (price-of m) rename-cost)
           :naming-cost rename-cost))

(defn- renamed [m input text]
  (let [result (:result m)]
    (cond
      (not (str/blank? text))
      (if (= text (stack/hover-name input))
        m
        (named m result text))
      (some? (stack/custom-name input)) (named m result nil)
      :else m)))

(defn- only-renaming? [m]
  (let [naming (long (:naming-cost m))]
    (and (= naming (price-of m)) (pos? naming))))

(defn- priced [m creative?]
  (let [price (price-of m)
        raw (if (pos? price)
              (min (+ (long (:tax m)) price) Integer/MAX_VALUE)
              0)
        only? (only-renaming? m)
        cost (if (and only? (>= raw too-expensive)) 39 raw)
        shown? (and (pos? price)
                    (or creative? (< cost too-expensive)))]
    (assoc m :cost cost :renaming? only?
             :result (when shown? (:result m)))))

(defn- taxed [m addition]
  (if-let [result (:result m)]
    (let [base (max (stack/repair-cost result)
                    (stack/repair-cost addition))
          grown (if (only-renaming? m)
                  base
                  (stack/increased-repair-cost base))]
      (assoc m :result
               (-> (stack/put result :repair-cost grown)
                   (stack/set-enchantments (:enchantments m)))))
    m))

(def ^:private empty-work
  {:result nil :cost 0 :price 0 :naming-cost 0 :repair-count 0
   :renaming? false})

(def ^:private fields
  [:result :cost :price :naming-cost :repair-count :renaming?])

(defn- started [input addition creative?]
  (assoc empty-work
    :result input :creative? creative?
    :tax (+ (stack/repair-cost input)
            (stack/repair-cost addition))
    :enchantments (stack/enchantments input)))

(defn- finished [m input addition text creative?]
  (if (:stop m)
    m
    (-> (renamed m input text)
        (priced creative?)
        (taxed addition))))

(defn work
  "Returns what an anvil offers for its inputs and a name."
  [input addition text creative?]
  (if (nil? input)
    empty-work
    (let [m (cond-> (started input addition creative?)
              addition (combined input addition))]
      (select-keys (finished m input addition text creative?)
                   fields))))

(def ^:private ^:const max-name-length 50)

(defn- allowed-char? [^long c]
  (and (>= c 32) (not= c 127) (not= c 167)))

(defn valid-name
  "Returns the name an anvil accepts from text, or nil."
  [text]
  (let [ok? #(allowed-char? (long (int %)))
        kept (apply str (filter ok? text))]
    (when (<= (count kept) max-name-length) kept)))

(def ^:private stages
  {:anvil :chipped-anvil :chipped-anvil :damaged-anvil})

(defn next-stage
  "Returns the block an anvil wears down to, or nil when it breaks."
  [b]
  (stages b))

(defn- menu-work [m inv creative?]
  (work (get inv 0) (get inv 1) (:name m) (boolean creative?)))

(defn- taken [m inv]
  (let [n (long (:repair-count m 0))
        left (- (long (:count (get inv 1) 1)) n)
        inv (cond
              (zero? n) (cond-> inv (not (:renaming? m)) (dissoc 1))
              (pos? left) (update inv 1 assoc :count left)
              :else (dissoc inv 1))]
    (dissoc inv 0)))

(defn- derive-result [m inv creative?]
  (if-let [r (:result (menu-work m inv creative?))]
    (assoc inv 2 r)
    (dissoc inv 2)))

(defn layout
  [m ctx]
  (let [place? (fn [slot _] (not= 2 (long slot)))
        base (menu/slots-layout 3 place?)
        v (:visible base)
        creative? (boolean (:infinite? ctx))]
    (assoc base
      :result 2
      :quick (fn [_ slot] (menu/combiner-quick v 2 slot))
      :on-take (fn [inv] (taken m inv))
      :derive (fn [inv] (derive-result m inv creative?)))))

(defn changed
  "Returns anvil menu m with the cost of the items in its slots."
  [m items ctx]
  (let [w (menu-work m items (:infinite? ctx))]
    (assoc m :cost (:cost w) :repair-count (:repair-count w)
             :renaming? (:renaming? w))))

(def ^:private ^:const wear-chance 0.12)

(defn- worn-state [^long st]
  (when-let [b (next-stage (block/block-of st))]
    (block/state b (block/props-of st))))

(defn- wear-deltas [world pos ^long st]
  (let [base (dec (long (:tick world)))
        worn (worn-state st)
        c [pos (or worn (block/emptied st))]
        event (if worn :sound-anvil-used :sound-anvil-broken)]
    (concat (changes/flagged-deltas world [c] (if worn 2 3) nil base)
            [(out/all (out/level-event event pos 0))])))

(defn take-deltas
  "Returns the wear and the sound of a result leaving an anvil."
  [world m creative?]
  (let [pos (:pos m)
        st (chest/state-at (:chunks world) pos)
        roll (random/of-key (:tick world) pos :anvil)]
    (if (and (not creative?) (< roll wear-chance))
      (wear-deltas world pos st)
      [(out/all (out/level-event :sound-anvil-used pos 0))])))
