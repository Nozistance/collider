(ns collider.game.menu
  "The inventory menu of a player: slots 0..45 as the vanilla InventoryMenu
   (0 crafting result, 1..4 crafting grid, 5..8 armor, 9..35 main, 36..44
   hotbar, 45 offhand), the carried stack, and what one click does to them.
   `click` is the vanilla AbstractContainerMenu.doClick for a creative player:
   it takes {:inventory :carried :quickcraft} and returns the same with
   :drops, the stacks thrown out."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def slot-count 46)
(def outside -999)
(def ^:private armor-slots {5 :head 6 :chest 7 :legs 8 :feet})
(def ^:private hotbar-slot 36)
(def ^:private offhand-slot 45)

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))
(defn- max-of ^long [s] (data/max-stack (:item s)))
(defn- same? [a b] (and a b (= (:item a) (:item b))))
(defn- sized [s ^long n] (when (pos? n) (assoc s :count n)))

(defn- may-place?
  "Whether the stack may go into the slot: nothing into the crafting result,
   only matching armor into armor slots."
  [slot stack]
  (cond
    (= 0 (long slot)) false
    (armor-slots slot) (= (armor-slots slot) (data/equip-slot (:item stack)))
    :else true))

(defn- insert
  "Puts up to n of stack into the slot; [inventory' left-over]."
  [inv slot stack ^long n]
  (let [here (get inv slot)]
    (if (or (not (may-place? slot stack)) (and here (not (same? here stack))))
      [inv stack]
      (let [room (- (max-of stack) (count-of here))
            put  (max 0 (min n (count-of stack) room))]
        [(if (pos? put) (assoc inv slot (sized stack (+ (count-of here) put))) inv)
         (sized stack (- (count-of stack) put))]))))

(defn- take-out
  "Takes up to n from the slot; [inventory' taken]."
  [inv slot ^long n]
  (let [here (get inv slot)
        got  (min n (count-of here))]
    (if (pos? got)
      [(if-let [left (sized here (- (count-of here) got))] (assoc inv slot left) (dissoc inv slot))
       (sized here got)]
      [inv nil])))

(defn- pickup [{:keys [inventory carried] :as m} slot primary?]
  (let [clicked (get inventory slot)]
    (cond
      (= outside (long slot))
      (if carried
        (let [n (if primary? (count-of carried) 1)]
          (assoc m :carried (sized carried (- (count-of carried) n)) :drops [(sized carried n)]))
        m)
      (nil? clicked)
      (if carried
        (let [[inv left] (insert inventory slot carried (if primary? (count-of carried) 1))]
          (assoc m :inventory inv :carried left))
        m)
      (nil? carried)
      (let [n (if primary? (count-of clicked) (quot (inc (count-of clicked)) 2))
            [inv got] (take-out inventory slot n)]
        (assoc m :inventory inv :carried got))
      (may-place? slot carried)
      (cond
        (same? clicked carried)
        (let [[inv left] (insert inventory slot carried (if primary? (count-of carried) 1))]
          (assoc m :inventory inv :carried left))
        (<= (count-of carried) (max-of carried))
        (assoc m :inventory (assoc inventory slot carried) :carried clicked)
        :else m)
      (same? clicked carried)
      (let [[inv got] (take-out inventory slot (min (count-of clicked) (- (max-of carried) (count-of carried))))]
        (assoc m :inventory inv :carried (sized carried (+ (count-of carried) (count-of got)))))
      :else m)))

(defn- move-to
  "Moves stack into the slots from..to (end exclusive), first onto the same
   stacks, then into empty slots; backwards? walks from the end.
   [inventory' left-over]."
  [inv stack from to backwards?]
  (let [from (long from) to (long to)
        slots (if backwards? (range (dec to) (dec from) -1) (range from to))
        [inv stack] (reduce (fn [[inv s :as acc] slot]
                              (cond
                                (nil? s) (reduced acc)
                                (same? (get inv slot) s) (insert inv slot s (count-of s))
                                :else acc))
                            [inv stack] slots)]
    (if (nil? stack)
      [inv nil]
      (reduce (fn [[inv s :as acc] slot]
                (if (and s (nil? (get inv slot)) (may-place? slot s))
                  (reduced (insert inv slot s (count-of s)))
                  acc))
              [inv stack] slots))))

(defn- quick-move-once [inv slot]
  (let [stack (get inv slot)
        equip (data/equip-slot (:item stack))
        armor (some (fn [[k v]] (when (= v equip) k)) armor-slots)
        inv'  (dissoc inv slot)
        [inv' left]
        (cond
          (nil? stack) [inv nil]
          (= 0 (long slot)) (move-to inv' stack 9 46 true)
          (< (long slot) 9) (move-to inv' stack 9 46 false)
          (and armor (nil? (get inv armor))) (move-to inv' stack armor (inc (long armor)) false)
          (and (= :offhand equip) (nil? (get inv offhand-slot))) (move-to inv' stack offhand-slot 46 false)
          (< (long slot) hotbar-slot) (move-to inv' stack hotbar-slot 45 false)
          (< (long slot) offhand-slot) (move-to inv' stack 9 hotbar-slot false)
          :else (move-to inv' stack 9 46 false))]
    (if left (assoc inv' slot left) inv')))

(defn- quick-move
  "Shift-click: the stack goes to its other place while any of it moves."
  [{:keys [inventory] :as m} slot]
  (loop [inv inventory]
    (let [inv' (quick-move-once inv slot)]
      (if (or (= inv' inv) (nil? (get inv' slot)))
        (assoc m :inventory inv')
        (recur inv')))))

(defn- swap-with
  "A number key or F: the slot and the hotbar or offhand slot trade stacks."
  [{:keys [inventory] :as m} slot ^long button]
  (let [other  (if (= 40 button) offhand-slot (+ hotbar-slot button))
        source (get inventory other)
        target (get inventory slot)]
    (cond
      (and (nil? source) (nil? target)) m
      (nil? source) (assoc m :inventory (-> inventory (assoc other target) (dissoc slot)))
      (not (may-place? slot source)) m
      (nil? target) (assoc m :inventory (-> inventory (assoc slot source) (dissoc other)))
      :else (assoc m :inventory (-> inventory (assoc slot source) (assoc other target))))))

(defn- clone [{:keys [inventory carried] :as m} slot]
  (if-let [here (and (nil? carried) (get inventory slot))]
    (assoc m :carried (assoc here :count (max-of here)))
    m))

(defn- throw-out [{:keys [inventory carried] :as m} slot ^long button]
  (if-let [here (and (nil? carried) (get inventory slot))]
    (let [n (if (zero? button) 1 (count-of here))
          [inv got] (take-out inventory slot n)]
      (assoc m :inventory inv :drops [got]))
    m))

(defn- pickup-all [{:keys [inventory carried] :as m} slot ^long button]
  (if (and carried (nil? (get inventory slot)))
    (let [slots (if (zero? button) (range slot-count) (range (dec slot-count) -1 -1))
          step (fn [[inv c] pass]
                 (reduce (fn [[inv c :as acc] s]
                           (let [here (get inv s)]
                             (if (and here (same? here c) (not= 0 (long s))
                                      (< (count-of c) (max-of c))
                                      (or (= 1 pass) (not= (count-of here) (max-of here))))
                               (let [[inv' got] (take-out inv s (min (count-of here) (- (max-of c) (count-of c))))]
                                 [inv' (sized c (+ (count-of c) (count-of got)))])
                               acc)))
                         [inv c] slots))
          [inv c] (step (step [inventory carried] 0) 1)]
      (assoc m :inventory inv :carried c))
    m))

(defn- spread
  "End of a drag: the carried stack spread over the dragged slots (vanilla
   quickcraft type 0 evenly, 1 one each, 2 a full stack each in creative)."
  [{:keys [inventory carried] :as m} slots ^long type]
  (let [n (count slots)
        each (case type 0 (quot (count-of carried) n) 1 1 (max-of carried))
        [inv left] (reduce (fn [[inv c] slot]
                             (if (and c (or (= type 2) (>= (count-of c) n)))
                               (let [[inv' left] (insert inv slot c each)]
                                 [inv' (if (= type 2) c left)])
                               [inv c]))
                           [inventory carried] (sort slots))]
    (assoc m :inventory inv :carried (if (= type 2) carried left) :quickcraft nil)))

(defn- quick-craft [{:keys [carried quickcraft] :as m} slot ^long button]
  (let [header (bit-and button 3) type (bit-and (bit-shift-right button 2) 3)
        status (long (:status quickcraft 0))]
    (cond
      (nil? carried) (assoc m :quickcraft nil)
      (= 0 header) (assoc m :quickcraft {:status 1 :type type :slots #{}})
      (and (= 1 header) (= 1 status))
      (let [here (get (:inventory m) slot)]
        (if (and (may-place? slot carried) (or (nil? here) (same? here carried))
                 (or (= 2 (long (:type quickcraft))) (> (count-of carried) (count (:slots quickcraft)))))
          (update-in m [:quickcraft :slots] conj slot)
          m))
      (and (= 2 header) (= 1 status))
      (let [slots (:slots quickcraft)]
        (cond
          (empty? slots) (assoc m :quickcraft nil)
          (= 1 (count slots)) (pickup (assoc m :quickcraft nil) (first slots) (= 0 (long (:type quickcraft))))
          :else (spread m slots (long (:type quickcraft)))))
      :else (assoc m :quickcraft nil))))

(defn click
  "One container click of the player on their own inventory: mode 0 pickup,
   1 quick move, 2 swap, 3 clone, 4 throw, 5 quick craft (drag), 6 pick up
   all, as the vanilla ContainerInput ids."
  [{:keys [quickcraft] :as m} {:keys [slot button mode]}]
  (let [m (assoc m :drops [])
        slot (long slot) button (long button) mode (long mode)
        in-range? (< -1 slot slot-count)]
    (cond
      (= 5 mode) (quick-craft m slot button)
      (:status quickcraft) (assoc m :quickcraft nil)
      (and (#{0 1} mode) (#{0 1} button) (= outside slot)) (pickup m slot (= 0 button))
      (not in-range?) m
      (= 0 mode) (if (#{0 1} button) (pickup m slot (= 0 button)) m)
      (= 1 mode) (if (#{0 1} button) (quick-move m slot) m)
      (= 2 mode) (if (or (< -1 button 9) (= 40 button)) (swap-with m slot button) m)
      (= 3 mode) (clone m slot)
      (= 4 mode) (throw-out m slot button)
      (= 6 mode) (pickup-all m slot button)
      :else m)))
