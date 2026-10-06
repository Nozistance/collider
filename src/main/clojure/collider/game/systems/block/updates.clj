(ns collider.game.systems.block.updates
  "Scheduled block ticks and their effects."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.game.block.blockentity :as be]
            [collider.game.block.lid :as lid]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.schedule :as schedule]
            [collider.game.areas :as areas]
            [collider.game.level :as level]
            [collider.parallel :as par]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]
            [collider.world.neighbors :as neighbors]
            [collider.world.rules :as rules]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:private lists
  {:block-ticks {:type-of block/block-of :due rules/cell-changes
                 :reach rules/reach :lit? rules/lit?
                 :isolation (constantly 1)}
   :fluid-ticks {:type-of liquid/fluid-of :due rules/fluid-changes
                 :reach rules/fluid-reach
                 :lit? (constantly false)
                 :isolation #(liquid/reach (:dim %))}})

(defn- again-at [k chunks ctx changes p st]
  (when (and (= :block-ticks k) (not-any? #(= p (first %)) changes))
    (rules/again-tick chunks st p (:tick ctx))))

(defn- ruled [w ctx k p st]
  (let [{:keys [due reach]} (lists k)
        chunks (:chunks w)
        changes (due chunks st p ctx)]
    {:reach (reach st ctx) :changes changes
     :again (again-at k chunks ctx changes p st)}))

(defn- ran
  [w ctx k [p ty]]
  (let [st (chunk/at (:chunks w) p)
        type-of (get-in lists [k :type-of])]
    (cond
      (not= ty (type-of st)) {:reach 0}
      (= :block-ticks k)
      (or (lid/recheck w p st) (ruled w ctx k p st))
      :else (ruled w ctx k p st))))

(defn- packed ^long [^long x ^long z]
  (bit-or (bit-shift-left x 32) (bit-and z 0xFFFFFFFF)))

(defn- column ^long [[x _ z]]
  (packed (long x) (long z)))

(defn- near? [^long r [x _ z] ^long c]
  (and (<= (Math/abs (- (long x) (bit-shift-right c 32))) r)
       (<= (Math/abs (- (long z) (long (unchecked-int c)))) r)))

(defn- columns-around [^long r [x _ z]]
  (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))]
    (column [(+ (long x) (long dx)) 0 (+ (long z) (long dz))])))

(defn- touched? [dirty ^long r p]
  (let [side (inc (* 2 r))]
    (boolean
      (if (< (count dirty) (* side side))
        (some #(near? r p %) dirty)
        (some #(contains? dirty %) (columns-around r p))))))

(defn- relit [{:keys [lit] :as pass}]
  (if (empty? lit)
    pass
    (let [w (:w pass)
          writes (mapv (fn [[p [old st]]] [p old st]) lit)
          sky? (:sky? w true)
          chunks (light/relight-batch (:chunks w) writes sky?)]
      (assoc pass :w (assoc w :chunks chunks) :lit {}))))

(defn- rerun [pass ctx k [p :as tick]]
  (let [st (chunk/at (:chunks (:w pass)) p)
        pass (if ((get-in lists [k :lit?]) st ctx) (relit pass) pass)]
    [pass (ran (:w pass) ctx k tick)]))

(defn- lit-writes [lit writes]
  (reduce (fn [m [p old st]]
            (assoc m p [(get-in m [p 0] old) st]))
          lit writes))

(defn- out-into [pass ds]
  (if (seq ds)
    (assoc pass :out (reduce changes/joined-into (:out pass) ds))
    pass))

(defn- written [world k pass settled author]
  (let [writes (:writes settled)
        ds (changes/settled-deltas world settled author)]
    (cond-> (-> pass
                (assoc-in [:w :chunks] (:chunks settled))
                (out-into ds))
      (:dirty pass)
      (update :dirty into (map (comp column first)) writes)
      (= :block-ticks k) (update :lit lit-writes writes))))

(defn- applied [world ctx k pass changes by]
  (if (empty? changes)
    pass
    (let [op #(vector :set % (neighbors/flags-of % update/all))
          ops (mapv op changes)]
      (written world k pass
               (neighbors/run (:chunks (:w pass)) ctx ops)
               {:with by}))))

(defn- again-deltas [[p] {:keys [again]}]
  (when again
    [[:schedule-ticks {again [(cell/pack p)]}]]))

(defn- stale? [pass [p] first-run]
  (or (nil? first-run)
      (touched? (:dirty pass) (:reach first-run) p)))

(defn- stepped
  [world ctx k pass [tick first-run]]
  (let [[pass r] (if (stale? pass tick first-run)
                   (rerun pass ctx k tick)
                   [pass first-run])
        by (nth tick 1)
        ds (concat (again-deltas tick r)
                   (delta/authored (:deltas r) nil by))
        pass (out-into pass ds)]
    (applied world ctx k pass (:changes r) by)))

(defn- ordered [world k active]
  (let [runs? #(areas/active-id? active %)
        pos (fn [[id ty]] [(cell/unpack id) ty])
        order (schedule/run-order (get world k) (:tick world) runs?)]
    (mapv pos order)))

(def ^:private around
  (vec (for [dx [-1 0 1] dz [-1 0 1]] [dx dz])))

(defn- bucket ^long [^long size [x _ z] [dx dz]]
  (packed (+ (Math/floorDiv (long x) size) (long dx))
          (+ (Math/floorDiv (long z) size) (long dz))))

(defn- crowded? [seen ^long size p]
  (some #(contains? seen (bucket size p %)) around))

(defn- lone-flags [^long r ticks]
  (let [size (+ r 2)]
    (loop [ts (seq ticks)
           flags (transient [])
           seen (transient (lm/long-set))]
      (if ts
        (let [p (nth (first ts) 0)]
          (recur (next ts)
                 (conj! flags (not (crowded? seen size p)))
                 (conj! seen (bucket size p [0 0]))))
        flags))))

(defn- first-runs [world ctx k ticks]
  (let [r ((get-in lists [k :isolation]) ctx)
        lone (persistent! (lone-flags r ticks))
        first-run #(when (lone %) (ran world ctx k (ticks %)))]
    (if (some true? lone)
      (par/pmapv first-run (vec (range (count ticks))))
      (vec (repeat (count ticks) nil)))))

(defn- ticks-run [world k ticks]
  (let [ctx (level/level-ctx world)
        firsts (first-runs world ctx k ticks)
        w (update world :chunks chunk/editable)
        dirty (when (some some? firsts) (lm/long-set))
        start {:w w :dirty dirty :lit {} :out (transient [])}]
    (persistent!
      (:out (reduce #(stepped world ctx k %1 %2) start
                    (mapv vector ticks firsts))))))

(defn- parked-ids [world active due]
  (let [chunks (:chunks world)
        loaded? #(contains? chunks (chunk/block-id-chunk %))
        skip? #(or (areas/active-id? active %) (not (loaded? %)))]
    (into [] (comp (map key) (remove skip?)) due)))

(defn- ticks-deltas [world k]
  (let [t (long (:tick world))
        due (schedule/due (get world k) t)]
    (when (seq due)
      (let [active (areas/ticking-chunks world)
            parked (parked-ids world active due)]
        (into [[:ticks-flushed k t parked]]
              (ticks-run world k (ordered world k active)))))))

(defn block-updates
  {:wake {:keys [[:block-ticks :queue]]}}
  [world _d]
  (deltas/of-vec (ticks-deltas world :block-ticks)))

(defn fluid-updates
  {:wake {:keys [[:fluid-ticks :queue]]}}
  [world _d]
  (deltas/of-vec (ticks-deltas world :fluid-ticks)))

(defn- announced [w events]
  (let [heard (areas/broadcast-chunks w)]
    (filter (fn [[cp _]] (contains? heard cp)) events)))

(defn- changed-out [w [cp cells]]
  (let [recs (chunk/states-at (:chunks w) cells)]
    (out/all (out/blocks-changed cp recs))))

(defn- entity-outs [w events]
  (for [[cp cells] events
        :let [es (be/in-chunk w cp)]
        :when es
        pos cells
        :when (get es pos)]
    (out/all (out/block-entity pos))))

(defn block-flush
  "Returns the effects that show the blocks changed this tick."
  {:wake {:keys [:changed-blocks]}}
  [w _d]
  (deltas/of-vec
    (when-let [events (:changed-blocks w)]
      (concat [[:changed-blocks-flushed]]
              (map #(changed-out w %) (announced w events))
              (entity-outs w events)))))
