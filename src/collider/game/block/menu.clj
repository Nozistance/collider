(ns collider.game.block.menu
  "Menus and the effect of a player's clicks on their slots."
  (:require [collider.data :as data]))

(set! *warn-on-reflection* true)

(def slot-count 46)

(def outside -999)

(def ^:private armor-slots {5 :head 6 :chest 7 :legs 8 :feet})

(def ^:private hotbar-slot 36)

(def ^:private offhand-slot 45)

(defn- count-of ^long [s] (if s (long (:count s 1)) 0))

(defn- max-of ^long [s] (data/max-stack (:item s)))

(defn- same? [a b] (and a b (= (dissoc a :count) (dissoc b :count))))

(defn- sized [s ^long n] (when (pos? n) (assoc s :count n)))

(defn- player-may-place? [slot stack]
  (cond
    (= 0 (long slot)) false
    (armor-slots slot) (= (armor-slots slot) (data/equip-slot (:item stack)))
    :else true))

(defn- player-quick-slots
  "Returns where a shift-click sends the stack, best slot first."
  [inv slot]
  (let [slot (long slot)
        stack (get inv slot)
        equip (data/equip-slot (:item stack))
        armor (some (fn [[k v]] (when (= v equip) k)) armor-slots)]
    (cond
      (= 0 slot) (range 44 8 -1)
      (< slot 9) (range 9 45)
      (and armor (nil? (get inv armor))) [armor]
      (and (= :offhand equip) (nil? (get inv offhand-slot))) [offhand-slot]
      (< slot hotbar-slot) (range hotbar-slot 45)
      (< slot offhand-slot) (range 9 hotbar-slot)
      :else (range 9 45))))

(def player-layout
  {:count   slot-count
   :visible (vec (range slot-count))
   :place   player-may-place?
   :swap    (fn ^long [^long button] (if (= 40 button) offhand-slot (+ hotbar-slot button)))
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
      :place   (fn [slot stack] (or (>= (long slot) n) (place slot stack)))
      :swap    (fn ^long [^long button] (+ n (if (= 40 button) offhand-slot (+ hotbar-slot button))))
      :quick   (fn [_ slot] (slots-quick menu index n slot))})))

(defn container-layout
  ([rows] (container-layout rows (fn [_ _] true)))
  ([^long rows place] (slots-layout (* 9 rows) place)))

(defn- layout-of [m] (or (:layout m) player-layout))

(defn- cap-of ^long [layout slot stack]
  (let [f (:max layout) n (when f (f slot stack))]
    (if n (min (max-of stack) (long n)) (max-of stack))))

(defn- insert [layout inv slot stack n]
  (let [here (get inv slot)]
    (if (or (not ((:place layout) slot stack))
            (and here (not (same? here stack))))
      [inv stack]
      (let [room (- (cap-of layout slot stack) (count-of here))
            put (max 0 (min (long n) (count-of stack) room))]
        [(if (pos? put) (assoc inv slot (sized stack (+ (count-of here) put))) inv)
         (sized stack (- (count-of stack) put))]))))

(defn- take-out
  ([layout inv slot n] (take-out layout inv slot n Long/MAX_VALUE))
  ([layout inv slot n mx]
   (let [here (get inv slot)
         whole (count-of here)
         got (long (if (= slot (:result layout))
                     (if (< (long mx) whole) 0 whole)
                     (min (long n) (long mx) whole)))]
     (if (pos? got)
       [(if-let [left (sized here (- (count-of here) got))] (assoc inv slot left) (dissoc inv slot))
        (sized here got)]
       [inv nil]))))

(defn- place-carried [m layout slot carried primary?]
  (let [[inv left] (insert layout (:inventory m) slot carried
                           (if primary? (count-of carried) 1))]
    (assoc m :inventory inv :carried left)))

(defn- drop-carried [m carried primary?]
  (let [n (if primary? (count-of carried) 1)]
    (-> (assoc m :carried (sized carried (- (count-of carried) n)))
        (update :drops conj (sized carried n)))))

(defn- take-carried [m layout slot clicked primary?]
  (let [n (if primary? (count-of clicked) (quot (inc (count-of clicked)) 2))
        [inv got] (take-out layout (:inventory m) slot n)]
    (assoc m :inventory inv :carried got :asked n)))

(defn- onto-slot [m layout slot clicked carried primary?]
  (cond
    (same? clicked carried) (place-carried m layout slot carried primary?)
    (<= (count-of carried) (cap-of layout slot carried))
    (assoc m :inventory (assoc (:inventory m) slot carried) :carried clicked)
    :else m))

(defn- gather-onto-carried [m layout slot clicked carried]
  (let [room (- (max-of carried) (count-of carried))
        [inv got] (take-out layout (:inventory m) slot (min (count-of clicked) room) room)]
    (assoc m :inventory inv :carried (sized carried (+ (count-of carried) (count-of got))))))

(defn- pickup [{:keys [inventory carried] :as m} slot primary?]
  (let [layout (layout-of m)
        clicked (get inventory slot)]
    (cond
      (= outside (long slot)) (if carried (drop-carried m carried primary?) m)
      (nil? clicked) (if carried (place-carried m layout slot carried primary?) m)
      (nil? carried) (take-carried m layout slot clicked primary?)
      ((:place layout) slot carried) (onto-slot m layout slot clicked carried primary?)
      (same? clicked carried) (gather-onto-carried m layout slot clicked carried)
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

(defn- quick-move [m slot]
  (let [layout (layout-of m)]
    (loop [m m]
      (let [inv (:inventory m)
            moved (quick-move-once layout inv slot)
            m' (settle (assoc m :inventory moved) inv)]
        (if (or (= moved inv) (nil? (get (:inventory m') slot)))
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

(defn- swap-with [{:keys [inventory] :as m} slot ^long button]
  (let [layout (layout-of m)
        other ((:swap layout) button)
        source (get inventory other)
        target (get inventory slot)]
    (cond
      (and (nil? source) (nil? target)) m
      (nil? source) (assoc m :inventory (-> inventory (assoc other target) (dissoc slot)))
      (not ((:place layout) slot source)) m
      (nil? target) (assoc m :inventory (-> inventory (assoc slot source) (dissoc other)))
      :else (assoc m :inventory (-> inventory (assoc slot source) (assoc other target))))))

(defn- clone [{:keys [inventory carried] :as m} slot]
  (if-let [here (and (nil? carried) (get inventory slot))]
    (assoc m :carried (assoc here :count (max-of here)))
    m))

(defn- throw-out [{:keys [inventory carried] :as m} slot ^long button]
  (if-let [here (and (nil? carried) (get inventory slot))]
    (let [n (if (zero? button) 1 (count-of here))
          [inv got] (take-out (layout-of m) inventory slot n)]
      (-> (assoc m :inventory inv :asked n)
          (update :drops conj got)))
    m))

(defn- craft-throw [m layout slot ^long button]
  (loop [m m]
    (let [before (:inventory m)
          got (get before slot)
          m' (-> (throw-out m slot button)
                 (craft-settle layout before)
                 (dissoc :asked))]
      (if (and got (= 1 button)
               (= (:item got) (:item (get (:inventory m') slot))))
        (recur m')
        (assoc m' :settled true)))))

(defn- craft-result? [m slot]
  (let [layout (layout-of m)]
    (and (:craft layout) (= slot (:result layout)))))

(defn- gather-pass [layout place slots [inv c] pass]
  (reduce (fn [[inv c :as acc] s]
            (let [here (get inv s)]
              (if (and here (same? here c) (place s c)
                       (< (count-of c) (max-of c))
                       (or (= 1 (long pass)) (not= (count-of here) (max-of here))))
                (let [[inv' got] (take-out layout inv s (min (count-of here) (- (max-of c) (count-of c))))]
                  [inv' (sized c (+ (count-of c) (count-of got)))])
                acc)))
          [inv c] slots))

(defn- pickup-all [{:keys [inventory carried] :as m} slot ^long button]
  (let [layout (layout-of m)]
    (if (and carried (nil? (get inventory slot)))
      (let [slots (cond->> (:visible layout)
                           (:result layout) (remove #(= % (:result layout)))
                           (not (zero? button)) reverse)
            pass (partial gather-pass layout (:place layout) slots)
            [inv c] (pass (pass [inventory carried] 0) 1)]
        (assoc m :inventory inv :carried c))
      m)))

(defn- spread [{:keys [inventory carried] :as m} slots ^long type]
  (let [layout (layout-of m)
        n (count slots)
        each (case type 0 (quot (count-of carried) n) 1 1 (max-of carried))
        [inv left] (reduce (fn [[inv c] slot]
                             (if (and c (or (= type 2) (>= (count-of c) n)))
                               (let [[inv' left] (insert layout inv slot c each)]
                                 [inv' (if (= type 2) c left)])
                               [inv c]))
                           [inventory carried] (sort slots))]
    (assoc m :inventory inv :carried (if (= type 2) carried left) :quickcraft nil)))

(defn- quick-craft-add [m place quickcraft slot carried]
  (let [here (get (:inventory m) slot)]
    (if (and (place slot carried) (or (nil? here) (same? here carried))
             (or (= 2 (long (:type quickcraft))) (> (count-of carried) (count (:slots quickcraft)))))
      (update-in m [:quickcraft :slots] conj slot)
      m)))

(defn- quick-craft-end [m quickcraft]
  (let [slots (:slots quickcraft)]
    (cond
      (empty? slots) (assoc m :quickcraft nil)
      (= 1 (count slots)) (pickup (assoc m :quickcraft nil) (first slots) (= 0 (long (:type quickcraft))))
      :else (spread m slots (long (:type quickcraft))))))

(defn- quick-craft
  "Returns the menu after a click in a drag.
  The click starts, continues or ends the drag."
  [{:keys [carried quickcraft] :as m} slot ^long button]
  (let [header (bit-and button 3) type (bit-and (bit-shift-right button 2) 3)
        status (long (:status quickcraft 0))]
    (cond
      (nil? carried) (assoc m :quickcraft nil)
      (= 0 header) (assoc m :quickcraft {:status 1 :type type :slots #{}})
      (and (= 1 header) (= 1 status))
      (quick-craft-add m (:place (layout-of m)) quickcraft slot carried)
      (and (= 2 header) (= 1 status)) (quick-craft-end m quickcraft)
      :else (assoc m :quickcraft nil))))

(defn- shift-click [m slot]
  (if (craft-result? m slot)
    (craft-quick-move m (layout-of m) slot)
    (quick-move m slot)))

(defn- throw-click [m slot button]
  (if (craft-result? m slot)
    (craft-throw m (layout-of m) slot button)
    (throw-out m slot button)))

(defn- click-mode [m slot button ^long mode]
  (case mode
    0 (if (#{0 1} button) (pickup m slot (= 0 button)) m)
    1 (if (#{0 1} button) (shift-click m slot) m)
    2 (if (or (< -1 (long button) 9) (= 40 button)) (swap-with m slot button) m)
    3 (clone m slot)
    4 (throw-click m slot button)
    6 (pickup-all m slot button)
    m))

(defn- click-acted [m quickcraft slot menu button mode in-range?]
  (cond
    (= 5 (long mode)) (quick-craft m slot button)
    (:status quickcraft) (assoc m :quickcraft nil)
    (and (#{0 1} mode) (#{0 1} button) (= outside (long menu))) (pickup m slot (= 0 button))
    (not in-range?) m
    :else (click-mode m slot button mode)))

(defn click [{:keys [quickcraft] :as m} {:keys [slot button mode]}]
  (let [m (assoc m :drops [] :takes 0 :crafted [] :spills [])
        before (:inventory m)
        visible (:visible (layout-of m))
        menu (long slot) button (long button) mode (long mode)
        in-range? (< -1 menu (count visible))
        slot (if in-range? (long (nth visible menu)) menu)
        m' (click-acted m quickcraft slot menu button mode in-range?)]
    (if (:settled m')
      (dissoc m' :settled)
      (settle m' before))))
