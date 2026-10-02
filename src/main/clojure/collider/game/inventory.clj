(ns collider.game.inventory
  "Stacks a player takes into its inventory and spends from hand."
  (:require [collider.data :as data]
            [collider.game.item :as item]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(defn same-stack?
  "Returns true when stacks a and b hold one item with one set of
  components."
  [a b]
  (and (= (:item a) (:item b)) (= (:components a) (:components b))))

(def ^:private slot-order
  (vec (concat (range 36 45) (range 9 36))))

(defn- topping-up? [cur stack ^long cap ^long n]
  (and (pos? n) cur (same-stack? cur stack)
       (< (long (:count cur 1)) cap)))

(defn- topped-up [chs cur slot ^long take]
  (conj chs [slot (update cur :count (fnil + 1) take)]))

(defn- fill-existing [inv stack ^long n]
  (let [cap (long (data/max-stack (:item stack)))
        step (fn [[chs n] slot]
               (let [n (long n) cur (get inv slot)
                     have (long (:count cur 1))
                     take (min (- cap have) n)]
                 (if (topping-up? cur stack cap n)
                   [(topped-up chs cur slot take) (- n take)]
                   [chs n])))]
    (reduce step [[] n] slot-order)))

(defn- first-empty-slot [inv changes]
  (first (remove #(or (get inv %) (some (fn [[s _]] (= s %)) changes))
                 slot-order)))

(defn add-stack
  "Returns the slot changes that fit stack into inv, and the rest.
  The rest is nil when the whole stack found room."
  [inv stack]
  (let [[changes n] (fill-existing inv stack (long (:count stack 1)))
        n (long n)]
    (if-let [slot (when (pos? n) (first-empty-slot inv changes))]
      [(conj changes [slot (assoc stack :count n)]) nil]
      [changes (when (pos? n) (assoc stack :count n))])))

(defn- holds? [inv stack]
  (some #(and (= (:item %) (:item stack))
              (= (:components %) (:components stack)))
        (vals inv)))

(defn- shrunk [stack ^long n]
  (let [left (- (long (:count stack 1)) n)]
    (when (pos? left) (assoc stack :count left))))

(defn kept
  "Returns the deltas that put stack into the inventory of player
  eid, and throw out what does not fit."
  [world eid e stack]
  (let [[changes left] (add-stack (:inventory e) stack)]
    (concat (for [[slot s] changes] [:set-slot eid slot s])
            (when left
              [[:spawn-entity (item/dropped world eid left)]]))))

(defn- emptied [world eid e hand made]
  (let [slot (player/hand-slot e hand)
        left (shrunk (player/hand-stack e hand) 1)]
    (if left
      (cons [:set-slot eid slot left] (kept world eid e made))
      [[:set-slot eid slot made]])))

(defn- creative-filled [world eid e stack always?]
  (when (or always? (not (holds? (:inventory e) stack)))
    (kept world eid e stack)))

(defn filled-result-deltas
  "Returns the deltas of the container in hand turning into stack.
  The last container becomes the filled item in the hand. From a
  larger stack the filled item goes to the inventory. In creative the
  container stays, and the filled item goes to the inventory only when
  the player holds none or always? is true."
  ([world eid stack]
   (filled-result-deltas world eid stack false :main))
  ([world eid stack always?]
   (filled-result-deltas world eid stack always? :main))
  ([world eid stack always? hand]
   (let [e (get-in world [:entities eid])]
     (if (player/infinite-materials? e)
       (creative-filled world eid e stack always?)
       (emptied world eid e hand stack)))))

(defn consume-deltas
  "Returns the deltas of spending n of the item in hand.
  A player with infinite materials spends nothing."
  [eid e hand ^long n]
  (when-not (player/infinite-materials? e)
    [[:set-slot eid (player/hand-slot e hand)
      (shrunk (player/hand-stack e hand) n)]]))

(defn- remainder-deltas [world eid e hand stack left]
  (let [over (dec (long (:count stack 1)))
        made {:item (:item left) :count (long (:count left 1))}]
    (if (pos? over)
      (cons [:set-slot eid (player/hand-slot e hand)
             (assoc stack :count over)]
            (kept world eid e made))
      [[:set-slot eid (player/hand-slot e hand) made]])))

(defn use-item-deltas
  "Returns the deltas of a player using one item from hand.
  Some items leave a remainder, such as an empty bucket after milk.
  The remainder takes the hand or goes to the inventory."
  [world eid e hand]
  (let [stack (player/hand-stack e hand)
        left (get-in (data/items) [(:item stack) :use-remainder])]
    (if (and left (not (player/infinite-materials? e)))
      (remainder-deltas world eid e hand stack left)
      (consume-deltas eid e hand 1))))

(defn- broken-deltas [eid e hand stack]
  (let [fx (out/status eid (if (= :off hand) :break-off :break-main))]
    [[:set-slot eid (player/hand-slot e hand) (shrunk stack 1)]
     (out/all fx) (out/to eid fx)]))

(defn hurt-item-deltas
  "Returns the deltas of wearing the item in hand by n points.
  An item worn past its last point breaks and leaves the hand. A
  player with infinite materials wears nothing out."
  [eid e hand ^long n]
  (let [stack (player/hand-stack e hand)
        worn (+ n (stack/damage stack))]
    (when (and (stack/damageable? stack)
               (not (player/infinite-materials? e)))
      (if (>= worn (stack/max-damage stack))
        (broken-deltas eid e hand stack)
        [[:set-slot eid (player/hand-slot e hand)
          (stack/with-damage stack worn)]]))))
