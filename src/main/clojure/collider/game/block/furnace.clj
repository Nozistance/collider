(ns collider.game.block.furnace
  "Furnace, blast furnace and smoker fuel, recipes and cooking."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]
            [collider.game.craft :as craft]))

(set! *warn-on-reflection* true)

(def ^:private recipe-types
  {:furnace :smelting :blast-furnace :blasting :smoker :smoking
   :campfire :campfire})

(defn- in-items [in]
  (if (map? in) (data/tag-values "item" (:tag in)) in))

(defn- index-recipe [m r]
  (reduce (fn [m item]
            (if (get-in m [(:type r) item])
              m
              (assoc-in m [(:type r) item] r)))
          m (in-items (:in r))))

(def ^:private ^:table index
  (delay (reduce index-recipe {} (data/cooking-recipes))))

(defn recipe
  "Returns the recipe a furnace of kind cooks stack by, if any."
  [kind stack]
  (when stack (get-in @index [(recipe-types kind) (:item stack)])))

(def ^:private ^:table recipe-xp
  (delay (into {} (map (juxt :id :xp)) (data/cooking-recipes))))

(defn- f32 ^double [^double x] (double (unchecked-float x)))

(defn- reward
  "Returns the points n cooks of recipe id are worth, as
  AbstractFurnaceBlockEntity.createExperience."
  ^long [id ^long n roll]
  (let [made (f32 (* (f32 n) (f32 (double (get @recipe-xp id 0.0)))))
        whole (Math/floor made)
        part (f32 (- made (f32 whole)))]
    (cond-> (long whole)
      (and (not (zero? part)) (< (f32 (roll id)) part)) inc)))

(defn award-deltas
  "Returns the orbs furnace e pays at pos for what it cooked since
  the last payout, one award for each recipe, as
  AbstractFurnaceBlockEntity.getRecipesToAwardAndPopExperience."
  [e pos roll salt]
  (for [[id n] (sort-by key (:used e))
        :let [pts (reward id (long n) roll)]
        :when (pos? pts)]
    [:xp-award (vec pos) pts [salt id]]))

(defn burn-duration ^long [stack]
  (long (get (data/fuel) (:item stack) 0)))

(defn- same? [a b] (= (dissoc a :count) (dissoc b :count)))

(defn- can-burn? [items result]
  (let [cur (nth items 2)]
    (cond
      (nil? cur) true
      (not (same? cur result)) false
      :else (<= (+ (long (:count cur 1)) (long (:count result 1)))
                (min 64 (data/max-stack (:item result)))))))

(defn- consume-fuel [items]
  (let [f (nth items 1)
        n (dec (long (:count f 1)))]
    (assoc items 1 (if (pos? n)
                     (assoc f :count n)
                     (craft/remainder f)))))

(defn- grown [cur result]
  (if cur
    (let [n (+ (long (:count cur 1)) (long (:count result 1)))]
      (assoc cur :count n))
    result))

(defn- burn [items result]
  (let [input (nth items 0)
        n (dec (long (:count input 1)))]
    (-> items
        (assoc 2 (grown (nth items 2) result))
        (cond-> (and (= :wet-sponge (:item input))
                     (= :bucket (:item (nth items 1))))
                (assoc 1 {:item :water-bucket :count 1}))
        (assoc 0 (when (pos? n) (assoc input :count n))))))

(defn- ignite [e]
  (let [n (burn-duration (nth (:items e) 1))]
    (cond-> (assoc e :lit-remaining n :lit-total n)
            (pos? n) (assoc :items (consume-fuel (:items e))))))

(defn- cook-step [e r]
  (let [c (inc (long (:cook e 0)))]
    (if (= c (long (:cook-total e 0)))
      (assoc e :cook 0 :cook-total (:time r)
               :items (burn (:items e) (:out r))
               :used (update (:used e) (:id r) (fnil inc 0)))
      (assoc e :cook c))))

(defn- active [e lit? r]
  (let [e (if lit? e (ignite e))
        lit? (or lit? (pos? (long (:lit-remaining e 0))))]
    [(if lit? (cook-step e r) (assoc e :cook 0)) lit?]))

(defn- run [e lit? r]
  (cond
    (nil? r) [e lit?]
    (can-burn? (:items e) (:out r)) (active e lit? r)
    :else [(assoc e :cook 0) lit?]))

(defn- cool [e]
  (let [c (long (:cook e 0))]
    (if (pos? c)
      (assoc e :cook (min (max 0 (- c 2)) (long (:cook-total e 0))))
      e)))

(defn- countdown [e]
  (let [n (long (:lit-remaining e 0))]
    (if (pos? n)
      [(assoc e :lit-remaining (dec n)) (> n 1)]
      [e false])))

(defn tick
  "Returns the furnace after one tick and whether it is lit."
  [e]
  (let [[e lit?] (countdown e)
        items (:items e)
        input (nth items 0)]
    (cond
      (not (or lit? (and (nth items 1) input))) [(cool e) lit?]
      (nil? input) [(assoc e :cook 0) lit?]
      :else (run e lit? (recipe (:kind e) input)))))

(defn input-changed
  "Returns the furnace with stack in its input slot.
  A different item restarts the cooking timer at the total time of
  its recipe."
  [e stack]
  (let [old (nth (:items e) 0)
        e (assoc-in e [:items 0] stack)]
    (if (and stack (same? old stack))
      e
      (assoc e :cook 0
               :cook-total (:time (recipe (:kind e) stack) 200)))))

(defn- fuel-slot-accepts? [stack]
  (or (pos? (burn-duration stack))
      (= :bucket (:item stack))))

(defn- may-place? [slot stack]
  (case (long slot) 0 true 1 (fuel-slot-accepts? stack) 2 false))

(defn- slot-max [slot stack]
  (when (and (= 1 (long slot)) (= :bucket (:item stack))) 1))

(defn- quick [v kind inv slot]
  (let [i (menu/index-of v slot)
        stack (get inv slot)]
    (cond
      (= 2 i) (menu/span v 3 39 true)
      (< i 2) (menu/span v 3 39 false)
      (recipe kind stack) (menu/span v 0 1 false)
      (pos? (burn-duration stack)) (menu/span v 1 2 false)
      (< 2 i 30) (menu/span v 30 39 false)
      :else (menu/span v 3 30 false))))

(defn layout
  "Returns the slot layout of furnace menu m."
  [m]
  (let [base (menu/slots-layout 3 may-place?)
        v (:visible base)
        kind (:type m)]
    (assoc base
      :max slot-max
      :stat {:slot 2 :by :removed}
      :quick (fn [inv slot] (quick v kind inv slot)))))
