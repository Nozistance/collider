(ns collider.game.block.anvil
  "What an anvil makes of two items and a name, and what it asks."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def ^:const too-expensive 40)

(def ^:private ^:const rename-cost 1)

(def ^:private ^:const sacrifice-cost 2)

(defn- of [name] (data/enchantment name))

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

(defn- mend
  "Returns the mended stack and the units of material it spends."
  [result ^long units]
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
  "Returns what an anvil offers for its inputs and a name.
  The map holds the result, its cost and what taking it spends."
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
  "Returns the block an anvil wears down to, nil when it breaks."
  [block]
  (stages block))
