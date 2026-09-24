(ns collider.game.block.menu
  "Menus and the effect of a player's clicks on their slots."
  (:require [collider.data :as data]
            [collider.game.stack :as stack]))

(set! *warn-on-reflection* true)

(def slot-count 46)

(def outside -999)

(def ^:private armor-slots {5 :head 6 :chest 7 :legs 8 :feet})

(def ^:private hotbar-slot 36)

(def ^:private offhand-slot 45)

(def ^:private container-max 99)

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))

(defn- max-of ^long [s] (stack/max-size s))

(defn- same? [a b] (and a b (= (dissoc a :count) (dissoc b :count))))

(defn- same-item? [a b] (and a b (= (:item a) (:item b))))

(defn- sized [s ^long n] (when (pos? n) (assoc s :count n)))

(defn- put-slot [inv slot s]
  (if s (assoc inv slot s) (dissoc inv slot)))

(defn- player-may-place? [slot stack]
  (let [part (armor-slots slot)]
    (cond
      (= 0 (long slot)) false
      part (= part (data/equip-slot (:item stack)))
      :else true)))

(defn- player-quick-slots
  "Returns where a shift-click sends the stack, best slot first."
  [inv slot]
  (let [slot (long slot)
        equip (data/equip-slot (:item (get inv slot)))
        armor (some (fn [[k v]] (when (= v equip) k)) armor-slots)
        free? #(nil? (get inv %))]
    (cond
      (= 0 slot) (range 44 8 -1)
      (< slot 9) (range 9 45)
      (and armor (free? armor)) [armor]
      (and (= :offhand equip) (free? offhand-slot)) [offhand-slot]
      (< slot hotbar-slot) (range hotbar-slot 45)
      (< slot offhand-slot) (range 9 hotbar-slot)
      :else (range 9 45))))

(defn- hand-slot ^long [^long base ^long button]
  (+ base (if (= 40 button) offhand-slot (+ hotbar-slot button))))

(def player-layout
  {:count   slot-count
   :visible (vec (range slot-count))
   :base    0
   :place   player-may-place?
   :max     (fn [slot _] (when (armor-slots slot) 1))
   :equip   (assoc armor-slots offhand-slot :offhand)
   :swap    (fn ^long [^long button] (hand-slot 0 button))
   :quick   player-quick-slots})

(defn- slots-menu [^long n]
  (into (vec (range n))
        (map #(+ n (long %)))
        (concat (range 9 36) (range 36 45))))

(defn- slots-quick [menu index ^long n slot]
  (let [m (long (index slot -1))]
    (cond
      (neg? m) nil
      (< m n) (map menu (range (dec (count menu)) (dec n) -1))
      :else (map menu (range 0 n)))))

(defn slots-layout
  ([n] (slots-layout n (fn [_ _] true)))
  ([^long n place]
   (let [menu (slots-menu n)
         index (into {} (map-indexed (fn [i f] [f i])) menu)]
     {:count   (+ n 46)
      :visible menu
      :base    n
      :place   (fn [slot s] (or (>= (long slot) n) (place slot s)))
      :swap    (fn ^long [^long button] (hand-slot n button))
      :quick   (fn [_ slot] (slots-quick menu index n slot))})))

(defn container-layout
  ([rows] (container-layout rows (fn [_ _] true)))
  ([^long rows place] (slots-layout (* 9 rows) place)))

(defn- layout-of [m] (or (:layout m) player-layout))

(defn- slot-max ^long [layout slot stack]
  (let [f (:max layout) n (when f (f slot stack))]
    (min (max-of stack) (long (or n container-max)))))

(defn- own-slot ^long [^long base ^long i]
  (+ base
     (cond (< i 9) (+ hotbar-slot i) (= i 40) offhand-slot :else i)))

(defn- room? [inv slot stack]
  (let [s (get inv slot)]
    (and (same? s stack) (> (max-of s) 1)
         (< (count-of s) (min container-max (max-of s))))))

(defn- space-slot [base held inv stack]
  (some #(when (room? inv % stack) %)
        (map #(own-slot base %) (list* held 40 (range 36)))))

(defn- free-slot [base inv]
  (some #(when-not (get inv %) %)
        (map #(own-slot base %) (range 36))))

(defn- add-once [base held inv stack]
  (if-let [slot (or (space-slot base held inv stack)
                    (free-slot base inv))]
    (let [here (count-of (get inv slot))
          cap (min container-max (max-of stack))
          put (min (count-of stack) (- cap here))]
      [(assoc inv slot (assoc stack :count (+ here put)))
       (- (count-of stack) put)])
    [inv (count-of stack)]))

(defn- add-all [base held inv stack]
  (loop [inv inv n (count-of stack)]
    (let [[inv' n'] (add-once base held inv (assoc stack :count n))
          n' (long n')]
      (if (and (pos? n') (< n' n))
        (recur inv' n')
        [inv' n']))))

(defn- damaged? [stack]
  (and (stack/damageable? stack) (pos? (stack/damage stack))))

(defn- add-damaged [base inv stack]
  (if-let [s (free-slot base inv)]
    [(assoc inv s stack) 0]
    [inv (count-of stack)]))

(defn add-to-inventory [base held creative? inv stack]
  (let [base (long base)
        [inv' n] (if (damaged? stack)
                   (add-damaged base inv stack)
                   (add-all base (long held) inv stack))]
    [inv' (when-not creative? (sized stack n))]))

(defn- insert [layout inv slot stack n]
  (let [here (get inv slot)
        have (count-of here)]
    (if (or (not ((:place layout) slot stack))
            (and here (not (same? here stack))))
      [inv stack]
      (let [room (- (slot-max layout slot stack) have)
            put (max 0 (min (long n) (count-of stack) room))
            inv (cond-> inv
                  (pos? put) (assoc slot (sized stack (+ have put))))]
        [inv (sized stack (- (count-of stack) put))]))))

(defn- removable [layout inv slot n mx]
  (let [here (get inv slot)
        whole (count-of here)
        n (long n) mx (long mx)]
    (cond
      (and (not ((:place layout) slot here)) (< mx whole)) 0
      (= slot (:result layout)) whole
      :else (min n mx whole))))

(defn- stat-slot? [layout slot]
  (= slot (get-in layout [:stat :slot])))

(defn- award [m item ^long n]
  (cond-> m (and item (pos? n)) (update :crafted conj [item n])))

(defn- take-out
  ([m slot n] (take-out m slot n Long/MAX_VALUE))
  ([m slot n mx]
   (let [layout (layout-of m)
         inv (:inventory m)
         here (get inv slot)
         got (long (removable layout inv slot n mx))
         left (sized here (- (count-of here) got))]
     (if (pos? got)
       [(cond-> (assoc m :inventory (put-slot inv slot left))
          (stat-slot? layout slot) (award (:item here) got))
        (sized here got)]
       [m nil]))))

(defn- place-carried [m slot carried primary?]
  (let [n (if primary? (count-of carried) 1)
        inv (:inventory m)
        [inv left] (insert (layout-of m) inv slot carried n)]
    (assoc m :inventory inv :carried left)))

(defn- drop-carried [m carried primary?]
  (let [n (if primary? (count-of carried) 1)]
    (-> (assoc m :carried (sized carried (- (count-of carried) n)))
        (update :drops conj (sized carried n)))))

(defn- take-carried [m slot clicked primary?]
  (let [all (count-of clicked)
        n (if primary? all (quot (inc all) 2))
        [m got] (take-out m slot n)]
    (assoc m :carried got :asked n)))

(defn- onto-slot [m layout slot clicked carried primary?]
  (cond
    (same? clicked carried) (place-carried m slot carried primary?)
    (<= (count-of carried) (slot-max layout slot carried))
    (assoc m :inventory (assoc (:inventory m) slot carried)
             :carried clicked)
    :else m))

(defn- gather-onto-carried [m slot clicked carried]
  (let [have (count-of carried)
        room (- (max-of carried) have)
        [m got] (take-out m slot (count-of clicked) room)]
    (assoc m :carried (sized carried (+ have (count-of got))))))

(defn- pickup [{:keys [inventory carried] :as m} slot primary?]
  (let [layout (layout-of m)
        clicked (get inventory slot)]
    (cond
      (= outside (long slot))
      (if carried (drop-carried m carried primary?) m)
      (neg? (long slot)) m
      (nil? clicked)
      (if carried (place-carried m slot carried primary?) m)
      (nil? carried) (take-carried m slot clicked primary?)
      ((:place layout) slot carried)
      (onto-slot m layout slot clicked carried primary?)
      (same? clicked carried)
      (gather-onto-carried m slot clicked carried)
      :else m)))

(defn- move-onto [layout inv stack slots]
  (reduce (fn [[inv s :as acc] slot]
            (cond
              (nil? s) (reduced acc)
              (same? (get inv slot) s)
              (insert layout inv slot s (count-of s))
              :else acc))
          [inv stack] slots))

(defn- move-into [layout inv stack slots]
  (reduce (fn [[inv s :as acc] slot]
            (if (and s (nil? (get inv slot))
                     ((:place layout) slot s))
              (reduced (insert layout inv slot s (count-of s)))
              acc))
          [inv stack] slots))

(defn- move-to [layout inv stack slots]
  (let [[inv stack] (move-onto layout inv stack slots)]
    (if (nil? stack)
      [inv nil]
      (move-into layout inv stack slots))))

(defn- move-quick [layout inv stack q]
  (if (map? q)
    (let [[inv' left :as moved] (move-to layout inv stack (:try q))]
      (if (and (= inv' inv) (= left stack))
        (move-to layout inv stack (:else q))
        moved))
    (move-to layout inv stack q)))

(defn- quick-move-once [layout inv slot]
  (let [stack (get inv slot)
        inv' (dissoc inv slot)
        q (when stack ((:quick layout) inv slot))
        [inv' left] (if (nil? stack)
                      [inv nil]
                      (move-quick layout inv' stack q))]
    (if left (assoc inv' slot left) inv')))

(defn- bench-settle [m layout before]
  (let [{:keys [result on-take derive]} layout
        took? (and result (some? (get before result))
                   (nil? (get (:inventory m) result)))
        inv (cond-> (:inventory m) took? on-take)]
    (cond-> (assoc m :inventory (derive inv))
      took? (update :takes inc))))

(defn- grid-changed? [layout before after]
  (not= (map before (:grid layout)) (map after (:grid layout))))

(defn- took [m layout got]
  (let [n (min (long (:asked m (count-of got))) (count-of got))]
    (-> m
        (update :crafted conj [(:item got) n])
        ((:craft layout))
        (update :takes inc))))

(defn- craft-settle [m layout before]
  (let [r (:result layout)
        got (get before r)
        taken? (and got (nil? (get (:inventory m) r)))
        m (cond-> m taken? (took layout got))]
    (cond-> m
      (grid-changed? layout before (:inventory m))
      (update :inventory (:derive layout)))))

(defn- settle [m before]
  (let [layout (layout-of m)]
    (dissoc (cond (:craft layout) (craft-settle m layout before)
                  (:derive layout) (bench-settle m layout before)
                  :else m)
            :asked)))

(defn- stat-by [layout] (get-in layout [:stat :by]))

(defn- result-left [m layout slot]
  (let [left (get (:inventory m) slot)
        n (if (= :taken (stat-by layout)) (count-of left) 0)]
    (cond-> (award m (:item left) n)
      (and left (= slot (:result layout)) (:on-take layout))
      (-> (update :inventory dissoc slot)
          (update :spills conj left)))))

(defn- quick-stat [m layout slot before]
  (let [was (get before slot)
        moved (- (count-of was) (count-of (get (:inventory m) slot)))]
    (cond
      (not (and (stat-slot? layout slot) (pos? moved))) m
      (= :removed (stat-by layout)) (award m (:item was) moved)
      :else (result-left m layout slot))))

(defn- quick-move [m slot]
  (let [layout (layout-of m)]
    (loop [m m]
      (let [inv (:inventory m)
            moved (quick-move-once layout inv slot)
            m' (-> (assoc m :inventory moved)
                   (quick-stat layout slot inv)
                   (settle inv))
            now (get (:inventory m') slot)]
        (if (or (= moved inv) (not (same-item? (get inv slot) now)))
          (assoc m' :settled true)
          (recur m'))))))

(defn- craft-once [m layout slot]
  (let [inv (:inventory m)
        stack (get inv slot)
        q ((:quick layout) inv slot)
        [inv' left] (move-quick layout (dissoc inv slot) stack q)
        moved (- (count-of stack) (count-of left))]
    (when (pos? moved)
      (cond-> (-> (assoc m :inventory inv')
                  (update :crafted conj [(:item stack) moved])
                  ((:craft layout))
                  (update :inventory (:derive layout))
                  (update :takes inc))
        left (update :spills conj left)))))

(defn- craft-quick-move [m layout slot]
  (loop [m m]
    (let [item (:item (get (:inventory m) slot))
          m' (when item (craft-once m layout slot))]
      (cond (nil? m') (assoc m :settled true)
            (= item (:item (get (:inventory m') slot))) (recur m')
            :else (assoc m' :settled true)))))

(defn- swap-out [m layout slot other target]
  (let [inv (-> (:inventory m) (assoc other target) (dissoc slot))]
    (cond-> (assoc m :inventory inv)
      (and (stat-slot? layout slot)
           (= :taken (get-in layout [:stat :by])))
      (award (:item target) (count-of target)))))

(defn- swap-in [m slot other source mx]
  (let [inv (:inventory m)
        mx (long mx)
        n (count-of source)]
    (assoc m :inventory
             (if (> n mx)
               (-> inv (assoc slot (sized source mx))
                   (assoc other (sized source (- n mx))))
               (-> inv (assoc slot source) (dissoc other))))))

(defn- swap-split [m layout slot other source target mx]
  (let [m (swap-in m slot other source mx)
        add (partial add-to-inventory (:base layout 0) (:held m 0))
        [inv left] (add (:creative? m) (:inventory m) target)]
    (cond-> (assoc m :inventory inv)
      left (update :drops conj left))))

(defn- swap-both [m layout slot other source target]
  (let [mx (slot-max layout slot source)]
    (if (> (count-of source) mx)
      (swap-split m layout slot other source target mx)
      (let [inv (-> (:inventory m) (assoc slot source)
                    (assoc other target))]
        (assoc m :inventory inv)))))

(defn- swap-with [{:keys [inventory] :as m} slot ^long button]
  (let [layout (layout-of m)
        other ((:swap layout) button)
        source (get inventory other)
        target (get inventory slot)
        m (assoc m :direct other)]
    (cond
      (and (nil? source) (nil? target)) m
      (nil? source) (swap-out m layout slot other target)
      (not ((:place layout) slot source)) m
      (nil? target)
      (swap-in m slot other source (slot-max layout slot source))
      :else (swap-both m layout slot other source target))))

(defn- clone [{:keys [inventory carried] :as m} slot]
  (if-let [here (and (:creative? m) (nil? carried)
                     (get inventory slot))]
    (assoc m :carried (assoc here :count (max-of here)))
    m))

(defn- throw-once [m slot n]
  (let [before (:inventory m)
        [m got] (take-out m slot n)]
    [(-> (cond-> m got (update :drops conj got))
         (assoc :asked n)
         (settle before))
     got]))

(defn- throw-out [{:keys [inventory carried] :as m} slot ^long button]
  (if-let [here (and (nil? carried) (get inventory slot))]
    (let [n (if (zero? button) 1 (count-of here))]
      (loop [m m]
        (let [[m' got] (throw-once m slot n)]
          (if (and got (= 1 button)
                   (same-item? got (get (:inventory m') slot)))
            (recur m')
            (assoc m' :settled true)))))
    m))

(defn- craft-result? [m slot]
  (let [layout (layout-of m)]
    (and (:craft layout) (= slot (:result layout)))))

(defn- gathers? [m s ^long pass]
  (let [c (:carried m)
        here (get (:inventory m) s)]
    (and (same? here c) (< (count-of c) (max-of c))
         (<= (count-of here) (max-of c))
         (or (= 1 pass) (not= (count-of here) (max-of here))))))

(defn- gather-one [m s]
  (let [c (:carried m)
        here (get (:inventory m) s)
        room (- (max-of c) (count-of c))
        [m got] (take-out m s (count-of here) room)]
    (assoc m :carried (sized c (+ (count-of c) (count-of got))))))

(defn- gather-pass [slots m pass]
  (reduce (fn [m s] (if (gathers? m s pass) (gather-one m s) m))
          m slots))

(defn- pickup-all [{:keys [inventory carried] :as m} slot button]
  (let [layout (layout-of m)
        skip (set (:no-gather layout))]
    (if (and carried (nil? (get inventory slot)))
      (let [slots (cond->> (remove skip (:visible layout))
                    (not (zero? (long button))) reverse)]
        (reduce (partial gather-pass slots) m [0 1]))
      m)))

(defn- quick-replace? [here c]
  (or (nil? here)
      (and (same? here c) (<= (count-of here) (max-of c)))))

(defn- drag-fits? [layout inv slot c n type]
  (and (quick-replace? (get inv slot) c)
       ((:place layout) slot c)
       (or (= 2 (long type)) (>= (count-of c) (long n)))))

(defn- drag-fill [layout c ^long n ^long type]
  (let [each (case type 0 (quot (count-of c) n) 1 1 (max-of c))]
    (fn [[inv ^long left :as acc] slot]
      (if (drag-fits? layout inv slot c n type)
        (let [carry (count-of (get inv slot))
              new (min (+ each carry) (slot-max layout slot c))]
          [(put-slot inv slot (sized c new)) (- left (- new carry))])
        acc))))

(defn- spread [m slots ^long type]
  (let [c (:carried m)
        fill (drag-fill (layout-of m) c (count slots) type)
        start [(:inventory m) (count-of c)]
        [inv left] (reduce fill start (sort slots))]
    (assoc m :inventory inv :carried (sized c left))))

(declare click-mode)

(defn- drag-end [m {:keys [slots type]}]
  (case (count slots)
    0 m
    1 (click-mode m (first slots) type 0)
    (spread m slots type)))

(defn- drag-add [m {:keys [slots type] :as qc} slot]
  (let [c (:carried m)]
    (if (and (not (neg? (long slot)))
             (quick-replace? (get (:inventory m) slot) c)
             ((:place (layout-of m)) slot c)
             (or (= 2 (long type)) (> (count-of c) (count slots))))
      (assoc m :quickcraft (update qc :slots conj slot))
      m)))

(defn- drag-start [m ^long type]
  (if (or (< type 2) (and (= 2 type) (:creative? m)))
    (assoc m :quickcraft {:status 1 :type type :slots #{}})
    m))

(defn- quick-craft
  "Returns the menu after a click in a drag.
  The click starts, continues or ends the drag."
  [{:keys [carried quickcraft] :as m} slot ^long button]
  (let [header (bit-and button 3)
        type (bit-and (bit-shift-right button 2) 3)
        expected (long (:status quickcraft 0))
        reset (assoc m :quickcraft nil)]
    (cond
      (not (or (= expected header) (= [1 2] [expected header]))) reset
      (nil? carried) reset
      (= 0 header) (drag-start reset type)
      (= 1 header) (drag-add m quickcraft slot)
      (= 2 header) (drag-end reset quickcraft)
      :else reset)))

(defn- shift-click [m slot]
  (if (craft-result? m slot)
    (craft-quick-move m (layout-of m) slot)
    (quick-move m slot)))

(defn- swap-key? [button]
  (or (< -1 (long button) 9) (= 40 (long button))))

(defn- click-mode [m slot button ^long mode]
  (case mode
    0 (if (#{0 1} button) (pickup m slot (= 0 button)) m)
    1 (if (#{0 1} button) (shift-click m slot) m)
    2 (if (swap-key? button) (swap-with m slot button) m)
    3 (clone m slot)
    4 (throw-out m slot button)
    6 (pickup-all m slot button)
    m))

(defn- click-acted [m quickcraft slot menu button mode in-range?]
  (cond
    (= 5 (long mode)) (quick-craft m slot button)
    (:status quickcraft) (assoc m :quickcraft nil)
    (and (#{0 1} mode) (#{0 1} button) (= outside (long menu)))
    (pickup m slot (= 0 button))
    (not in-range?) m
    :else (click-mode m slot button mode)))

(defn- put-on? [m slot part a b]
  (and b (not= slot (:direct m)) (not (same? a b))
       (= part (data/equip-slot (:item b)))))

(defn- equipped [m before]
  (let [after (:inventory m)]
    (keep (fn [[slot part]]
            (let [b (get after slot)]
              (when (put-on? m slot part (get before slot) b) b)))
          (sort (:equip (layout-of m))))))

(defn click [{:keys [quickcraft] :as m} {:keys [slot button mode]}]
  (let [m (assoc m :drops [] :takes 0 :crafted [] :spills [])
        before (:inventory m)
        visible (:visible (layout-of m))
        menu (long slot) button (long button) mode (long mode)
        in-range? (< -1 menu (count visible))
        slot (if in-range? (long (nth visible menu)) menu)
        m' (click-acted m quickcraft slot menu button mode in-range?)
        m' (if (:settled m') (dissoc m' :settled) (settle m' before))]
    (dissoc (assoc m' :equipped (vec (equipped m' before))) :direct)))
