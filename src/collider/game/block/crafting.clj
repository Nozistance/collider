(ns collider.game.block.crafting
  "Crafting grids of the table and the player, their results and the
  cost of a take."
  (:require [collider.data :as data]
            [collider.game.block.menu :as menu]
            [collider.game.craft :as craft]
            [collider.game.state :as state])
  (:import (java.util List)))

(set! *warn-on-reflection* true)

(def ^:private special
  #{:banner-duplicate :book-cloning :firework-rocket :firework-star
    :firework-star-fade :repair-item :map-extending
    :shield-decoration :decorated-pot})

(defn context [world e]
  {:limited?  (boolean (get-in world [:rules :limited-crafting]))
   :known     (or (:known-recipes e) #{})
   :held      (long (or (:held-slot e) 0))
   :infinite? (state/infinite-materials? e)})

(defn- allowed? [ctx recipe]
  (or (contains? special (:type recipe))
      (not (:limited? ctx))
      (contains? (:known ctx) (:id recipe))))

(defn- input-of [inv grid ^long w]
  (craft/trim {:w w :h w :stacks (mapv #(get inv %) grid)}))

(defn result [ctx stacks ^long w]
  (let [input (craft/trim {:w w :h w :stacks (vec stacks)})]
    (when-let [r (craft/find (craft/index) input nil)]
      (when (allowed? ctx r)
        (craft/assemble r input)))))

(defn- deriving [ctx ^long slot grid ^long w]
  (fn [inv]
    (if-let [r (result ctx (map #(get inv %) grid) w)]
      (assoc inv slot r)
      (dissoc inv slot))))

(defn- remaining [input]
  (if-let [r (craft/find (craft/index) input nil)]
    (craft/remainders r input)
    (:stacks input)))

(defn- same-kind? [a b]
  (= (dissoc a :count) (dissoc b :count)))

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))

(defn- max-of ^long [s] (long (data/max-stack (:item s))))

(defn- own-slot ^long [ctx ^long i]
  (+ (long (:base ctx 0))
     (cond (< i 9) (+ 36 i) (= i 40) 45 :else i)))

(defn- room? [inv slot stack]
  (let [s (get inv slot)]
    (and s (same-kind? s stack) (> (max-of s) 1)
         (< (count-of s) (max-of s)))))

(defn- space-slot [ctx inv stack]
  (some #(when (room? inv % stack) %)
        (map #(own-slot ctx %)
             (list* (:held ctx) 40 (range 36)))))

(defn- free-slot [ctx inv]
  (some #(when-not (get inv %) %)
        (map #(own-slot ctx %) (range 36))))

(defn- add-once [ctx inv stack]
  (if-let [slot (or (space-slot ctx inv stack) (free-slot ctx inv))]
    (let [here (count-of (get inv slot))
          put (min (count-of stack) (- (max-of stack) here))]
      [(assoc inv slot (assoc stack :count (+ here put)))
       (- (count-of stack) put)])
    [inv (count-of stack)]))

(defn- added [ctx inv stack]
  (loop [inv inv n (count-of stack)]
    (let [[inv' n'] (add-once ctx inv (assoc stack :count n))
          n' (long n')]
      (if (and (pos? n') (< n' n))
        (recur inv' n')
        [inv' n']))))

(defn- damaged? [stack]
  (pos? (long (get-in stack [:components :damage] 0))))

(defn- added-damaged [ctx inv stack]
  (if-let [slot (free-slot ctx inv)]
    [(assoc inv slot stack) 0]
    [inv (count-of stack)]))

(defn- give [ctx m stack]
  (let [add (if (damaged? stack) added-damaged added)
        [inv ^long n] (add ctx (:inventory m) stack)
        m (assoc m :inventory inv)]
    (if (or (zero? n) (:infinite? ctx))
      m
      (update m :spills conj (assoc stack :count n)))))

(defn- shrink [inv slot]
  (let [s (get inv slot)
        n (dec (count-of s))]
    (cond (nil? s) inv
          (pos? n) (assoc inv slot (assoc s :count n))
          :else (dissoc inv slot))))

(defn- replaced [ctx m slot rep]
  (let [here (get (:inventory m) slot)]
    (cond (nil? rep) m
          (nil? here) (assoc-in m [:inventory slot] rep)
          (same-kind? here rep)
          (assoc-in m [:inventory slot]
                    (assoc rep :count (+ (count-of rep)
                                         (count-of here))))
          :else (give ctx m rep))))

(defn- cell-slot [grid ^long w input ^long i]
  (let [iw (long (:w input))
        x (+ (rem i iw) (long (:left input)))
        y (+ (quot i iw) (long (:top input)))]
    (nth grid (+ x (* y w)))))

(defn- consuming [ctx grid ^long w]
  (fn [m]
    (let [input (input-of (:inventory m) grid w)
          reps (remaining input)]
      (reduce (fn [m i]
                (let [slot (cell-slot grid w input i)]
                  (replaced ctx (update m :inventory shrink slot)
                            slot (nth reps i))))
              m
              (range (count reps))))))

(defn place-back
  "Returns a player's inventory with a stack put back into it as a
  closing screen puts it, and what did not fit."
  [ctx inv stack]
  (loop [inv inv s stack]
    (if-let [slot (and s (or (space-slot ctx inv s)
                             (free-slot ctx inv)))]
      (let [here (count-of (get inv slot))
            put (min (count-of s) (- (max-of s) here))
            left (- (count-of s) put)]
        (recur (assoc inv slot (assoc s :count (+ here put)))
               (when (pos? left) (assoc s :count left))))
      [inv s])))

(def ^:private player-grid [1 2 3 4])

(defn player-layout [ctx]
  (assoc menu/player-layout
    :result 0
    :grid player-grid
    :derive (deriving ctx 0 player-grid 2)
    :craft (consuming ctx player-grid 2)))

(defn- span [v ^long from ^long to reverse?]
  (map v (if reverse?
           (range (dec to) (dec from) -1)
           (range from to))))

(defn- table-quick [v slot]
  (let [i (long (.indexOf ^List v slot))]
    (cond
      (= 0 i) (span v 10 46 true)
      (< i 10) (span v 10 46 false)
      :else {:try  (span v 1 10 false)
             :else (if (< i 37)
                     (span v 37 46 false)
                     (span v 10 37 false))})))

(def ^:private table-grid (vec (range 1 10)))

(defn table-layout [ctx]
  (let [base (menu/slots-layout 10 (fn [slot _] (not= 0 (long slot))))
        v (:visible base)
        ctx (assoc ctx :base 10)]
    (assoc base
      :result 0
      :grid table-grid
      :quick (fn [_ slot] (table-quick v slot))
      :derive (deriving ctx 0 table-grid 3)
      :craft (consuming ctx table-grid 3))))
