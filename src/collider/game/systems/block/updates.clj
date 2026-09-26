(ns collider.game.systems.block.updates
  "Scheduled block ticks and their effects."
  (:require [clojure.data.int-map :as i]
            [collider.game.block.blockentity :as be]
            [collider.game.deltas :as deltas]
            [collider.game.out :as out]
            [collider.game.schedule :as schedule]
            [collider.game.state :as state]
            [collider.game.systems.blocks.edit :as edit]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.env.weather :as weather]
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private lists
  {:block-wakes {:type-of block/block-of :due rules/reshape-changes
                 :reach rules/reach :lit? rules/lit?}
   :block-ticks {:type-of block/block-of :due rules/cell-changes
                 :reach rules/reach :lit? rules/lit?}
   :fluid-ticks {:type-of liquid/fluid-of :due rules/fluid-changes
                 :reach rules/fluid-reach
                 :lit? (constantly false)}})

(defn- player-positions [world]
  (mapv (comp :pos val) (state/player-entries world)))

(defn- tick-ctx [world]
  (merge (select-keys world weather/fields)
         {:rules (:rules world)
          :dim (:dim world)
          :tick (long (:tick world))
          :time-of-day (:time-of-day world 0)
          :players (player-positions world)}))

(defn- again-at [k chunks ctx changes p st]
  (when (and (= :block-ticks k) (not-any? #(= p (first %)) changes))
    (rules/again-tick chunks st p (:tick ctx))))

(defn- ran
  "Returns the tick of type ty at p run on level w: how far across
  it reads, its changes and the tick it asks for again. A block no
  longer of its type skips it, as tickBlock and tickFluid do."
  [w ctx k [p ty]]
  (let [{:keys [type-of due reach]} (lists k)
        chunks (:chunks w)
        st (chunk/chunks-get-block chunks p)]
    (if (= ty (type-of st))
      (let [changes (due chunks st p ctx)]
        {:reach (reach st ctx) :changes changes
         :again (again-at k chunks ctx changes p st)})
      {:reach 0})))

(defn- column ^long [[x _ z]]
  (bit-or (bit-shift-left (long x) 32)
          (bit-and (long z) 0xFFFFFFFF)))

(defn- near? [^long r [x _ z] ^long c]
  (and (<= (Math/abs (- (long x) (bit-shift-right c 32))) r)
       (<= (Math/abs (- (long z) (long (unchecked-int c)))) r)))

(defn- columns-around [^long r [x _ z]]
  (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))]
    (column [(+ (long x) (long dx)) 0 (+ (long z) (long dz))])))

(defn- touched?
  "Tells whether a block was written this pass within r columns of
  p, at any height."
  [dirty ^long r p]
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
  (let [st (chunk/chunks-get-block (:chunks (:w pass)) p)
        pass (if ((get-in lists [k :lit?]) st ctx) (relit pass) pass)]
    [pass (ran (:w pass) ctx k tick)]))

(defn- lit-writes [lit writes]
  (reduce (fn [m [p old st]]
            (assoc m p [(get-in m [p 0] old) st]))
          lit writes))

(defn- applied [pass records]
  (if (empty? records)
    pass
    (let [changes (edit/block-changes records)
          [chunks writes] (state/blocks-changed (:w pass) changes)]
      (-> pass
          (assoc-in [:w :chunks] chunks)
          (update :dirty into (map (comp column first)) writes)
          (update :lit lit-writes writes)))))

(defn- change-deltas [world records]
  (into [[:set-blocks (edit/block-changes records)]]
        (edit/change-fx world records)))

(defn- tick-deltas [world [p] {:keys [changes again]}]
  (cond-> []
    again (conj [:schedule-ticks {again [(chunk/block-pos->id p)]}])
    (seq changes) (into (change-deltas world changes))))

(defn- stepped
  "Returns the pass after one tick. The tick keeps what it did on
  the level at the start unless a block it reads was written
  since; then it runs again on the level as it is now."
  [world ctx k pass [tick first-run]]
  (let [reach (:reach first-run)
        [pass r] (if (touched? (:dirty pass) reach (first tick))
                   (rerun pass ctx k tick)
                   [pass first-run])]
    (-> (applied pass (:changes r))
        (update :out into (tick-deltas world tick r)))))

(defn- ordered [world k active]
  (let [runs? #(state/active-id? active %)
        pos (fn [[id ty]] [(chunk/id->block-pos id) ty])
        order (schedule/run-order (get world k) (:tick world) runs?)]
    (mapv pos order)))

(defn- ticks-run
  "Returns the deltas of the ticks, each run on the level the ticks
  before it left, as LevelTicks runs them. All run at once on the
  level at the start first; only those that read what an earlier
  one wrote run again."
  [world k ticks]
  (let [ctx (tick-ctx world)
        firsts (deltas/pmapcat (fn [t] [(ran world ctx k t)]) ticks)
        start {:w world :dirty (i/int-set) :lit {} :out []}]
    (:out (reduce #(stepped world ctx k %1 %2) start
                  (map vector ticks firsts)))))

(defn- parked-ids
  "Returns the due ids of loaded chunks that do not tick now."
  [world active due]
  (let [chunks (:chunks world)
        loaded? #(contains? chunks (chunk/block-id-chunk %))
        skip? #(or (state/active-id? active %) (not (loaded? %)))]
    (into [] (comp (map key) (remove skip?)) due)))

(defn- ticks-deltas [world k]
  (let [t (long (:tick world))
        due (schedule/due (get world k) t)]
    (when (seq due)
      (let [active (state/ticking-chunks world)
            parked (parked-ids world active due)]
        (into [[:ticks-flushed k t parked]]
              (ticks-run world k (ordered world k active)))))))

(defn neighbor-updates
  "Runs the neighbour updates that are due. They go before the
  block ticks, which then see what the updates did."
  [world _d]
  [#(ticks-deltas world :block-wakes)])

(defn block-updates
  "Runs the block ticks that are due."
  [world _d]
  [#(ticks-deltas world :block-ticks)])

(defn fluid-updates [world _d]
  [#(ticks-deltas world :fluid-ticks)])

(defn- final-records
  "Returns the last state of each block.
  The order is the one in which the blocks first changed."
  [recs]
  (let [end (into {} recs)
        final (fn [pos] [pos (get end pos)])]
    (into [] (comp (map first) (distinct) (map final)) recs)))

(defn- announced
  "Keeps the changes clients hear about.
  A chunk that is only full, never block ticking, is silent."
  [w events]
  (let [heard (state/broadcast-chunks w)]
    (filter (fn [[cp _]] (contains? heard cp)) events)))

(defn- changed-out [[cp recs]]
  (out/all (out/blocks-changed cp (final-records recs))))

(defn- entity-outs [w events]
  (for [[_ recs] events
        [pos _] recs
        :when (be/at w pos)]
    (out/all (out/block-entity pos))))

(defn block-flush
  "Tells the clients about the blocks that changed this tick."
  [w _d]
  (when-let [events (:block-events w)]
    (concat [[:block-events-flushed]]
            (map changed-out (announced w events))
            (entity-outs w events))))
