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
    (cond (== -1.0 (double hardness)) false
          (= :block push) false
          (= :destroy push) destroy?
          (= :push-only push) (= d along)
          :else (not entity?))))

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

(defn- line [chunks p start d]
  (loop [q start ps []]
    (let [st (chunk/at chunks q)]
      (cond (block/air-type? st) {:push ps :destroy []}
            (#{:slime-block :honey-block} (block/block-of st))
            (throw (ex-info "slime and honey do not move yet" {:pos q}))
            (or (not (pushable? st q d true d)) (= q p)) nil
            (destroyed? st) {:push ps :destroy [q]}
            (>= (count ps) limit) nil
            :else (recur (dir/toward q d) (conj ps q))))))

(defn resolved
  "Returns the cells the piston at p facing f pushes as it extends,
  as {:push cells :destroy cells} nearest first, or nil when it
  cannot."
  [chunks p f]
  (let [start (dir/toward p f)
        st (chunk/at chunks start)]
    (if (pushable? st start f false f)
      (line chunks p start f)
      (when (destroyed? st) {:push [] :destroy [start]}))))

(defn- block-entity [{:keys [block-entities]} p]
  (get-in block-entities [(chunk/block-chunk p) p]))

(defn- still-moving?
  [chunks p f {:keys [tick] :as ctx}]
  (let [q (dir/toward p f 2)
        st (chunk/at chunks q)
        e (block-entity ctx q)]
    (and (moving? st) (= f (block/facing-of st))
         (= :piston (:kind e)) (:extending? e)
         (or (zero? (long (:progress-o e))) (= tick (:ticked e))))))

(defn- event [st f b0]
  [:queue (block/block-of st) b0 (dir/index f)])

(defn- checked [chunks p ctx]
  (let [st (chunk/at chunks p)
        f (block/facing-of st)
        on (powered? chunks p f)]
    (cond
      (and on (not (extended? st)) (resolved chunks p f))
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

(defn fits?
  "Returns true when base is the extended piston behind head."
  [^long head ^long base]
  (and (base? base) (extended? base)
       (= (:type (block/props-of head))
          (if (= :sticky-piston (block/block-of base))
            :sticky
            :normal))
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
          (cond-> [[:break b]] drops? (conj [:drop b]))]]))))
