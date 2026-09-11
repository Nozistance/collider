(ns collider.game.menu
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

(defn- player-may-place? [slot stack]
  (cond
    (= 0 (long slot)) false
    (armor-slots slot) (= (armor-slots slot) (data/equip-slot (:item stack)))
    :else true))

(defn- player-quick-slots [inv slot]
  (let [slot  (long slot)
        stack (get inv slot)
        equip (data/equip-slot (:item stack))
        armor (some (fn [[k v]] (when (= v equip) k)) armor-slots)]
    (cond
      (= 0 slot) (range 45 8 -1)
      (< slot 9) (range 9 46)
      (and armor (nil? (get inv armor))) [armor]
      (and (= :offhand equip) (nil? (get inv offhand-slot))) [offhand-slot]
      (< slot hotbar-slot) (range hotbar-slot 45)
      (< slot offhand-slot) (range 9 hotbar-slot)
      :else (range 9 46))))

(def player-layout
  {:count   slot-count
   :visible (vec (range slot-count))
   :place   player-may-place?
   :swap    (fn ^long [^long button] (if (= 40 button) offhand-slot (+ hotbar-slot button)))
   :quick   player-quick-slots})

(defn container-layout
  ([rows] (container-layout rows (fn [_ _] true)))
  ([^long rows place]
   (let [n (* 9 rows)
         menu (into (vec (range n))
                    (map #(+ n (long %)))
                    (concat (range 9 36) (range 36 45)))
         index (into {} (map-indexed (fn [i f] [f i])) menu)]
     {:count   (+ n 46)
      :visible menu
      :place   (fn [slot stack] (or (>= (long slot) n) (place slot stack)))
      :swap    (fn ^long [^long button] (+ n (if (= 40 button) offhand-slot (+ hotbar-slot button))))
      :quick   (fn [_ slot]
                 (let [m (long (index slot -1))]
                   (cond
                     (neg? m) nil
                     (< m n) (map menu (range (dec (count menu)) (dec n) -1))
                     :else (map menu (range 0 n)))))})))

(defn- layout-of [m] (or (:layout m) player-layout))

(defn- insert [place inv slot stack n]
  (let [here (get inv slot)]
    (if (or (not (place slot stack)) (and here (not (same? here stack))))
      [inv stack]
      (let [room (- (max-of stack) (count-of here))
            put  (max 0 (min (long n) (count-of stack) room))]
        [(if (pos? put) (assoc inv slot (sized stack (+ (count-of here) put))) inv)
         (sized stack (- (count-of stack) put))]))))

(defn- take-out [inv slot ^long n]
  (let [here (get inv slot)
        got  (min n (count-of here))]
    (if (pos? got)
      [(if-let [left (sized here (- (count-of here) got))] (assoc inv slot left) (dissoc inv slot))
       (sized here got)]
      [inv nil])))

(defn- pickup [{:keys [inventory carried] :as m} slot primary?]
  (let [place   (:place (layout-of m))
        clicked (get inventory slot)]
    (cond
      (= outside (long slot))
      (if carried
        (let [n (if primary? (count-of carried) 1)]
          (assoc m :carried (sized carried (- (count-of carried) n)) :drops [(sized carried n)]))
        m)
      (nil? clicked)
      (if carried
        (let [[inv left] (insert place inventory slot carried (if primary? (count-of carried) 1))]
          (assoc m :inventory inv :carried left))
        m)
      (nil? carried)
      (let [n (if primary? (count-of clicked) (quot (inc (count-of clicked)) 2))
            [inv got] (take-out inventory slot n)]
        (assoc m :inventory inv :carried got))
      (place slot carried)
      (cond
        (same? clicked carried)
        (let [[inv left] (insert place inventory slot carried (if primary? (count-of carried) 1))]
          (assoc m :inventory inv :carried left))
        (<= (count-of carried) (max-of carried))
        (assoc m :inventory (assoc inventory slot carried) :carried clicked)
        :else m)
      (same? clicked carried)
      (let [[inv got] (take-out inventory slot (min (count-of clicked) (- (max-of carried) (count-of carried))))]
        (assoc m :inventory inv :carried (sized carried (+ (count-of carried) (count-of got)))))
      :else m)))

(defn- move-to [place inv stack slots]
  (let [[inv stack] (reduce (fn [[inv s :as acc] slot]
                              (cond
                                (nil? s) (reduced acc)
                                (same? (get inv slot) s) (insert place inv slot s (count-of s))
                                :else acc))
                            [inv stack] slots)]
    (if (nil? stack)
      [inv nil]
      (reduce (fn [[inv s :as acc] slot]
                (if (and s (nil? (get inv slot)) (place slot s))
                  (reduced (insert place inv slot s (count-of s)))
                  acc))
              [inv stack] slots))))

(defn- quick-move-once [layout inv slot]
  (let [stack (get inv slot)
        inv'  (dissoc inv slot)
        [inv' left] (if (nil? stack)
                      [inv nil]
                      (move-to (:place layout) inv' stack ((:quick layout) inv slot)))]
    (if left (assoc inv' slot left) inv')))

(defn- quick-move [{:keys [inventory] :as m} slot]
  (let [layout (layout-of m)]
    (loop [inv inventory]
      (let [inv' (quick-move-once layout inv slot)]
        (if (or (= inv' inv) (nil? (get inv' slot)))
          (assoc m :inventory inv')
          (recur inv'))))))

(defn- swap-with [{:keys [inventory] :as m} slot ^long button]
  (let [layout (layout-of m)
        other  ((:swap layout) button)
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
          [inv got] (take-out inventory slot n)]
      (assoc m :inventory inv :drops [got]))
    m))

(defn- pickup-all [{:keys [inventory carried] :as m} slot ^long button]
  (let [layout (layout-of m)
        place  (:place layout)]
    (if (and carried (nil? (get inventory slot)))
      (let [slots (cond-> (:visible layout) (not (zero? button)) reverse)
            step (fn [[inv c] pass]
                   (reduce (fn [[inv c :as acc] s]
                             (let [here (get inv s)]
                               (if (and here (same? here c) (place s c)
                                        (< (count-of c) (max-of c))
                                        (or (= 1 pass) (not= (count-of here) (max-of here))))
                                 (let [[inv' got] (take-out inv s (min (count-of here) (- (max-of c) (count-of c))))]
                                   [inv' (sized c (+ (count-of c) (count-of got)))])
                                 acc)))
                           [inv c] slots))
            [inv c] (step (step [inventory carried] 0) 1)]
        (assoc m :inventory inv :carried c))
      m)))

(defn- spread [{:keys [inventory carried] :as m} slots ^long type]
  (let [place (:place (layout-of m))
        n (count slots)
        each (case type 0 (quot (count-of carried) n) 1 1 (max-of carried))
        [inv left] (reduce (fn [[inv c] slot]
                             (if (and c (or (= type 2) (>= (count-of c) n)))
                               (let [[inv' left] (insert place inv slot c each)]
                                 [inv' (if (= type 2) c left)])
                               [inv c]))
                           [inventory carried] (sort slots))]
    (assoc m :inventory inv :carried (if (= type 2) carried left) :quickcraft nil)))

(defn- quick-craft [{:keys [carried quickcraft] :as m} slot ^long button]
  (let [place (:place (layout-of m))
        header (bit-and button 3) type (bit-and (bit-shift-right button 2) 3)
        status (long (:status quickcraft 0))]
    (cond
      (nil? carried) (assoc m :quickcraft nil)
      (= 0 header) (assoc m :quickcraft {:status 1 :type type :slots #{}})
      (and (= 1 header) (= 1 status))
      (let [here (get (:inventory m) slot)]
        (if (and (place slot carried) (or (nil? here) (same? here carried))
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

(defn click [{:keys [quickcraft] :as m} {:keys [slot button mode]}]
  (let [m (assoc m :drops [])
        layout (layout-of m)
        visible (:visible layout)
        menu (long slot) button (long button) mode (long mode)
        in-range? (< -1 menu (count visible))
        slot (if in-range? (long (nth visible menu)) menu)]
    (cond
      (= 5 mode) (quick-craft m slot button)
      (:status quickcraft) (assoc m :quickcraft nil)
      (and (#{0 1} mode) (#{0 1} button) (= outside menu)) (pickup m slot (= 0 button))
      (not in-range?) m
      (= 0 mode) (if (#{0 1} button) (pickup m slot (= 0 button)) m)
      (= 1 mode) (if (#{0 1} button) (quick-move m slot) m)
      (= 2 mode) (if (or (< -1 button 9) (= 40 button)) (swap-with m slot button) m)
      (= 3 mode) (clone m slot)
      (= 4 mode) (throw-out m slot button)
      (= 6 mode) (pickup-all m slot button)
      :else m)))
