(ns collider.game.block.enchanting
  "The enchanting table: the power of the bookshelves around it, the
  offers a player's seed makes and the enchanting of the item."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]
            [collider.game.enchantment :as enchantment]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.blocks.chest :as chest]))

(set! *warn-on-reflection* true)

(def ^:private ^:const multiplier 0x5DEECE66D)

(def ^:private ^:const mask 0xFFFFFFFFFFFF)

(defn- seeded [^long seed]
  (volatile! (bit-and (bit-xor seed multiplier) mask)))

(defn- bits
  "Returns the next n bits of r (LegacyRandomSource.next)."
  ^long [r ^long n]
  (let [s (-> (unchecked-multiply (long @r) multiplier)
              (unchecked-add 11)
              (bit-and mask))]
    (vreset! r s)
    (long (unchecked-int (bit-shift-right s (- 48 n))))))

(defn- below
  "Returns a number from 0 below bound (BitRandomSource.nextInt)."
  ^long [r ^long bound]
  (if (zero? (bit-and bound (dec bound)))
    (bit-shift-right (* bound (bits r 31)) 31)
    (loop []
      (let [s (bits r 31) m (rem s bound)]
        (if (neg? (unchecked-int (+ (- s m) (dec bound))))
          (recur)
          m)))))

(defn- fraction ^double [r]
  (float (* (double (bits r 24)) (double (float 5.9604645E-8)))))

(def ^:private offsets
  (for [x (range -2 3) y (range 0 2) z (range -2 3)
        :when (or (= 2 (abs x)) (= 2 (abs z)))]
    [x y z]))

(defn- block-tag [tag] (set (data/tag-values "block" tag)))

(def ^:private ^:table providers
  (delay (block-tag "enchantment_power_provider")))

(def ^:private ^:table transmitters
  (delay (block-tag "enchantment_power_transmitter")))

(defn- tagged? [tag chunks pos]
  (contains? @tag (block/block-of (chest/state-at chunks pos))))

(defn- shelf? [chunks pos [x y z]]
  (let [half [(quot (long x) 2) y (quot (long z) 2)]]
    (and (tagged? providers chunks (mapv + pos [x y z]))
         (tagged? transmitters chunks (mapv + pos half)))))

(defn shelves
  "Returns how many bookshelves power the table at pos
  (EnchantingTableBlock.isValidBookShelf)."
  ^long [world pos]
  (count (filter #(shelf? (:chunks world) pos %) offsets)))

(defn enchantable?
  "Returns true when stack may go under the table's enchanting."
  [stack]
  (and (some? (stack/component stack :enchantable))
       (= {} (stack/component stack :enchantments))))

(defn- slot-cost
  "EnchantmentHelper.getEnchantmentCost."
  ^long [r ^long slot ^long shelves]
  (let [b (min shelves 15)
        n (+ (below r 8) 1 (bit-shift-right b 1) (below r (inc b)))]
    (case slot
      0 (max (quot n 3) 1)
      1 (inc (quot (* n 2) 3))
      (max n (* b 2)))))

(defn- spread ^long [r ^long cost ^long enchantability]
  (let [q (inc (quot enchantability 4))
        c (+ cost 1 (below r q) (below r q))
        a (fraction r)
        span (float (* (float (- (float (+ a (fraction r))) 1.0))
                       (double (float 0.15))))
        v (float (+ (double c) (float (* (double c) span))))]
    (max 1 (long (Math/floor (+ v 0.5))))))

(defn- cost-at ^long [{:keys [base per-level]} ^long level]
  (+ (long base) (* (long per-level) (dec level))))

(defn- fitting-level [e ^long value]
  (some (fn [level]
          (when (<= (cost-at (:min-cost e) level) value
                    (cost-at (:max-cost e) level))
            level))
        (range (:max-level e) 0 -1)))

(defn- primary? [e item]
  (and (contains? (:supported e) item)
       (or (nil? (:primary e)) (contains? (:primary e) item))))

(def ^:private ^:table on-table
  (delay (data/tag-values "enchantment" "in_enchanting_table")))

(defn- available
  "EnchantmentHelper.getAvailableEnchantmentResults."
  [^long value item]
  (into []
        (keep (fn [k]
                (let [e (enchantment/info k)]
                  (when (or (= :book item) (primary? e item))
                    (some->> (fitting-level e value) (vector k))))))
        @on-table))

(defn- weight ^long [[k]] (long (:weight (enchantment/info k))))

(defn- pick [r options]
  (let [total (transduce (map weight) + options)]
    (when (pos? total)
      (loop [i (below r total) [o & more] options]
        (let [i (- i (weight o))]
          (if (neg? i) o (recur i more)))))))

(defn- compatible? [[a] [b]]
  (and (not= a b)
       (not (contains? (:exclusive (enchantment/info a)) b))
       (not (contains? (:exclusive (enchantment/info b)) a))))

(defn- more [r picked options ^long value]
  (if (<= (below r 50) value)
    (let [options (filterv #(compatible? (peek picked) %) options)]
      (if (empty? options)
        picked
        (recur r (conj picked (pick r options)) options
               (quot value 2))))
    picked))

(defn- selected
  "EnchantmentHelper.selectEnchantment."
  [r stack ^long cost]
  (if-let [n (stack/component stack :enchantable)]
    (let [value (spread r cost (long n))
          options (available value (:item stack))]
      (if (empty? options)
        []
        (more r [(pick r options)] options value)))
    []))

(defn- without [v ^long i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn- offered
  "Returns the random after, and the enchantments of the offer
  (EnchantmentMenu.getEnchantmentList)."
  [^long seed ^long slot ^long cost stack]
  (let [r (seeded (unchecked-int (+ seed slot)))
        xs (selected r stack cost)]
    [r (if (and (= :book (:item stack)) (> (count xs) 1))
         (without xs (below r (count xs)))
         xs)]))

(def blank
  "The offers of a table with nothing to enchant."
  {:costs [0 0 0] :clues [-1 -1 -1] :levels [-1 -1 -1]})

(defn- clue [seed stack ^long slot ^long cost]
  (when (pos? cost)
    (let [[r xs] (offered seed slot cost stack)]
      (when (seq xs) (nth xs (below r (count xs)))))))

(defn- costs [r ^long shelves]
  (mapv (fn [^long i]
          (let [c (slot-cost r i shelves)] (if (< c (inc i)) 0 c)))
        (range 3)))

(defn offers
  "Returns the costs and the enchantment and level shown on each
  of the three offers (EnchantmentMenu.slotsChanged)."
  [seed shelves stack]
  (if-not (and stack (enchantable? stack))
    blank
    (let [cs (costs (seeded seed) shelves)
          shown (mapv #(clue seed stack % (cs %)) (range 3))
          id #(data/entry-id "enchantment" %)]
      {:costs cs
       :clues (mapv #(if % (id (nth % 0)) -1) shown)
       :levels (mapv #(if % (nth % 1) -1) shown)})))

(defn enchanted
  "Returns stack with the enchantments of offer slot at cost, or nil
  when the offer holds none."
  [seed slot cost stack]
  (let [[_ xs] (offered seed slot cost stack)]
    (when (seq xs)
      (let [s (cond-> stack
                (= :book (:item stack))
                (stack/transmute :enchanted-book))
            levels (into (stack/enchantments s) xs)]
        (stack/set-enchantments s levels)))))

(defn next-seed
  "Returns the enchantment seed a roll from 0 to 1 makes."
  ^long [^double roll]
  (unchecked-int (long (Math/floor (* roll 4294967296.0)))))

(defn- may-place? [slot stack]
  (or (zero? (long slot)) (= :lapis-lazuli (:item stack))))

(defn- slot-max [slot _] (when (zero? (long slot)) 1))

(defn- quick [v inv slot]
  (let [i (menu/index-of v slot)]
    (cond
      (< i 2) (menu/span v 2 38 true)
      (= :lapis-lazuli (:item (get inv slot))) (menu/span v 1 2 true)
      :else (menu/span v 0 1 false))))

(defn layout
  "Returns the slot layout of an enchanting table menu."
  []
  (let [base (menu/slots-layout 2 may-place?)
        v (:visible base)]
    (assoc base
      :max slot-max
      :derive identity
      :quick (fn [inv slot] (quick v inv slot)))))

(defn changed
  "Returns enchanting menu m with the offers for its slots items and
  the bookshelves of the player context ctx."
  [m items ctx]
  (if (= (:contents m) items)
    m
    (let [shelves (:shelves ctx 0)]
      (merge m (offers (:seed m) shelves (nth items 0))))))
