(ns collider.game.block.furnace
  "Furnaces, blast furnaces and smokers: fuel, recipes and cooking."
  (:require [collider.data :as data]
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
  "Runs one server tick, returning the furnace and whether it is lit."
  [e]
  (let [[e lit?] (countdown e)
        items (:items e)
        input (nth items 0)]
    (cond
      (not (or lit? (and (nth items 1) input))) [(cool e) lit?]
      (nil? input) [(assoc e :cook 0) lit?]
      :else (run e lit? (recipe (:kind e) input)))))

(defn input-changed
  "Sets the input slot to stack. A different item restarts the cooking
  timer at the total time of its recipe."
  [e stack]
  (let [old (nth (:items e) 0)
        e (assoc-in e [:items 0] stack)]
    (if (and stack (same? old stack))
      e
      (assoc e :cook 0
               :cook-total (:time (recipe (:kind e) stack) 200)))))
