(ns collider.world.blocks.piston
  "The piston and its head, when the piston moves and what it moves."
  (:require [collider.data :as data]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:const limit
  "How many blocks a piston moves at most."
  12)

(defn- info [^long st] (get (data/blocks) (block/block-of st)))

(defn base? [^long st] (= :piston-base (block/type-of st)))

(defn head? [^long st] (= :piston-head (block/type-of st)))

(defn moving? [^long st] (= :moving-piston (block/type-of st)))

(defn extended? [^long st] (= :true (:extended (block/props-of st))))

(defn- fed? [chunks p d]
  (pos? (signal/signal chunks (dir/toward p d) d)))

(defn powered?
  "Returns true when the piston at p facing f has a signal beside it
  or beside the cell above it."
  [chunks p f]
  (let [a (dir/up p)]
    (boolean
      (or (some #(and (not= f %) (fed? chunks p %)) dir/six)
          (some #(and (not= :down %) (fed? chunks a %)) dir/six)))))

(def ^:private unmovable
  #{:obsidian :crying-obsidian :respawn-anchor :reinforced-deepslate})

(defn- edge? [y d]
  (or (and (= :down d) (== chunk/min-y (long y)))
      (and (= :up d) (== chunk/max-y (long y)))))

(defn- reaction [st d destroy? along]
  (let [{:keys [hardness push entity?]} (info st)]
    (if (== -1.0 (double hardness))
      false
      (case push
        :block false
        :destroy destroy?
        :push-only (= d along)
        :normal (not entity?)
        (throw (ex-info "unknown push reaction" {:state st}))))))

(defn pushable?
  "Returns true when a piston can move st at p toward d. A block that
  breaks counts when destroy? is true. One that only goes along a
  line counts when d is along."
  [st [_ y] d destroy? along]
  (cond (not (chunk/in-range? (long y))) false
        (block/air-type? (long st)) true
        (unmovable (block/block-of st)) false
        (edge? y d) false
        (base? st) (not (or (extended? st) (:entity? (info st))))
        :else (reaction st d destroy? along)))

(defn- destroyed? [st] (= :destroy (:push (info st))))

(defn- sticky? [^long st]
  (contains? #{:slime-block :honey-block} (block/block-of st)))

(defn- stick? [^long a ^long b]
  (and (not= #{:slime-block :honey-block}
             (hash-set (block/block-of a) (block/block-of b)))
       (or (sticky? a) (sticky? b))))

(defn- tail
  "Returns how many blocks the line from start takes, start and the
  sticky ones behind it, or nil past the limit."
  [{:keys [p d] :as r} s start]
  (let [back (dir/opposite d)]
    (loop [n 1 prev (chunk/at (:chunks r) start)]
      (if-not (sticky? prev)
        n
        (let [q (dir/toward start back n)
              st (chunk/at (:chunks r) q)]
          (cond (or (block/air-type? st) (not (stick? prev st))
                    (not (pushable? st q d false back)) (= q p)) n
                (> (+ n 1 (count (:push s))) limit) nil
                :else (recur (inc n) st)))))))

(defn- reordered [{:keys [push] :as s} ^long added ^long c]
  (let [cut (- (count push) added)]
    (assoc s :push (-> (subvec push 0 c)
                       (into (subvec push cut))
                       (into (subvec push c cut))))))

(declare line)

(defn- across? [d side]
  (and (not= side d) (not= side (dir/opposite d))))

(defn- branched [{:keys [chunks d] :as r} s q]
  (let [st (chunk/at chunks q)]
    (reduce (fn [s side]
              (let [n (dir/toward q side)
                    nst (chunk/at chunks n)]
                (if (and (across? d side) (stick? nst st))
                  (or (line r s n side) (reduced nil))
                  s)))
            s dir/six)))

(defn- collided [r s ^long c ^long added]
  (reduce (fn [s j]
            (let [q (nth (:push s) j)]
              (if (sticky? (chunk/at (:chunks r) q))
                (or (branched r s q) (reduced nil))
                s)))
          (reordered s added c) (range (inc (+ c added)))))

(defn- ahead [{:keys [p d] :as r} s start added]
  (loop [i 1 s s added (long added)]
    (let [q (dir/toward start d i)
          ps (:push s)
          c (.indexOf ^java.util.List ps q)
          st (chunk/at (:chunks r) q)]
      (cond (> c -1) (collided r s c added)
            (block/air-type? st) s
            (or (not (pushable? st q d true d)) (= q p)) nil
            (destroyed? st) (update s :destroy conj q)
            (>= (count ps) limit) nil
            :else (recur (inc i) (assoc s :push (conj ps q))
                         (inc added))))))

(defn- tail-cells [start d n]
  (map #(dir/toward start (dir/opposite d) %)
       (range (dec (long n)) -1 -1)))

(defn- line [{:keys [p d] :as r} s start side]
  (let [st (chunk/at (:chunks r) start)]
    (cond (or (block/air-type? st) (= start p)
              (.contains ^java.util.List (:push s) start)
              (not (pushable? st start d false side))) s
          (> (inc (count (:push s))) limit) nil
          :else (when-let [n (tail r s start)]
                  (let [ps (into (:push s) (tail-cells start d n))]
                    (ahead r (assoc s :push ps) start n))))))

(defn- spread [r s]
  (loop [i 0 s s]
    (if (or (nil? s) (>= i (count (:push s))))
      s
      (let [q (nth (:push s) i)]
        (recur (inc i) (if (sticky? (chunk/at (:chunks r) q))
                         (branched r s q)
                         s))))))

(defn resolved
  "Returns the cells the piston at p facing f moves, as it extends
  when extending? is true and as it pulls back otherwise, as {:push
  cells :destroy cells} in the order the piston takes them, or nil
  when it cannot."
  [chunks p f extending?]
  (let [d (if extending? f (dir/opposite f))
        start (dir/toward p f (if extending? 1 2))
        r {:chunks chunks :p p :d d}
        st (chunk/at chunks start)]
    (if (pushable? st start d false f)
      (spread r (line r {:push [] :destroy []} start d))
      (when (and extending? (destroyed? st))
        {:push [] :destroy [start]}))))

(defn- block-entity [{:keys [block-entities]} p]
  (get-in block-entities [(chunk/block-chunk p) p]))

(defn- still-moving?
  [chunks p f {:keys [tick] :as ctx}]
  (let [q (dir/toward p f 2)
        st (chunk/at chunks q)
        e (block-entity ctx q)]
    (and (moving? st) (= f (block/facing-of st))
         (= :piston (:kind e)) (:extending? e)
         (or (zero? (long (:progress-o e))) (= tick (:ticked e))
             (:handling-tick? ctx)))))

(defn- event [st f b0]
  [:queue (block/block-of st) b0 (dir/index f)])

(defn- checked [chunks p ctx]
  (let [st (chunk/at chunks p)
        f (block/facing-of st)
        on (powered? chunks p f)]
    (cond
      (and on (not (extended? st)) (resolved chunks p f true))
      [[p st [(event st f 0)]]]
      (and (not on) (extended? st))
      (let [b0 (if (still-moving? chunks p f ctx) 2 1)]
        [[p st [(event st f b0)]]]))))

(defn- placed? [chunks p {:keys [old]}]
  (not= (block/block-of (long old))
        (block/block-of (chunk/at chunks p))))

(def rule
  "The block rule of the piston base. It checks its signal on a
  neighbour update and as it is placed."
  {:name    :piston
   :match?  (fn [_chunks st _p] (base? st))
   :pass    :neighbor
   :wake    (constantly :neighbor)
   :reshape (fn [chunks p ctx]
              (when (or (not= :place (:pass ctx))
                        (placed? chunks p ctx))
                (checked chunks p ctx)))})

(defn type-of
  "Returns the type the head and the moving block of piston st take."
  [^long st]
  (if (= :sticky-piston (block/block-of st)) :sticky :normal))

(defn fits?
  "Returns true when base is the extended piston behind head."
  [^long head ^long base]
  (and (base? base) (extended? base)
       (= (:type (block/props-of head)) (type-of base))
       (= (block/facing-of head) (block/facing-of base))))

(defn- behind [p st]
  (dir/toward p (dir/opposite (block/facing-of st))))

(defn- survives? [chunks p st]
  (let [b (chunk/at chunks (behind p st))]
    (or (fits? st b)
        (and (moving? b)
             (= (block/facing-of st) (block/facing-of b))))))

(defn- head-heard [chunks p {:keys [pass side old]}]
  (let [st (chunk/at chunks p)
        back (dir/opposite (block/facing-of st))]
    (case pass
      :shape (when (and (= side back) (not (survives? chunks p st)))
               [(block/destroyed p st)])
      :neighbor (when (survives? chunks p st)
                  [[:changed (behind p st) old
                    (block/facing-of st)]]))))

(def head-rule
  "The block rule of the piston head. It goes with its base and hands
  neighbour updates to it."
  {:name    :piston-head
   :match?  (fn [_chunks st _p] (head? st))
   :pass    :both
   :place?  false
   :wake    (constantly :neighbor)
   :reshape head-heard})

(defn base-broken
  "Returns the step that breaks the base of head st at p, with its
  drops when drops? is true."
  [p ^long st drops?]
  (fn [chunks]
    (let [q (behind p st)
          b (chunk/at chunks q)]
      (when (fits? st b)
        [[q (block/emptied b)
          (cond-> [[:break b] [:game-event :block-destroy nil]]
            drops? (conj [:drop b]))]]))))
