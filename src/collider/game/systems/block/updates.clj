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
            [collider.world.chunk :as chunk]
            [collider.world.light :as light]
            [collider.world.neighbors :as neighbors]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private lists
  "The lists of what is due, each run by its own rules."
  {:block-ticks {:type-of block/block-of :due rules/cell-changes
                 :reach rules/reach :lit? rules/lit?
                 :crowd (constantly 1)}
   :fluid-ticks {:type-of liquid/fluid-of :due rules/fluid-changes
                 :reach rules/fluid-reach
                 :lit? (constantly false)
                 :crowd #(liquid/reach (:dim %))}})

(defn- again-at [k chunks ctx changes p st]
  (when (and (= :block-ticks k) (not-any? #(= p (first %)) changes))
    (rules/again-tick chunks st p (:tick ctx))))

(defn- ran
  [w ctx k [p ty]]
  (let [{:keys [type-of due reach]} (lists k)
        chunks (:chunks w)
        st (chunk/chunks-get-block chunks p)]
    (if (= ty (type-of st))
      (let [changes (due chunks st p ctx)]
        {:reach (reach st ctx) :changes changes
         :again (again-at k chunks ctx changes p st)})
      {:reach 0})))

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
  (let [st (chunk/chunks-get-block (:chunks (:w pass)) p)
        pass (if ((get-in lists [k :lit?]) st ctx) (relit pass) pass)]
    [pass (ran (:w pass) ctx k tick)]))

(defn- lit-writes [lit writes]
  (reduce (fn [m [p old st]]
            (assoc m p [(get-in m [p 0] old) st]))
          lit writes))

(defn- out-into [pass ds]
  (if (seq ds)
    (assoc pass :out (reduce edit/joined-into (:out pass) ds))
    pass))

(defn- written [world k pass s]
  (let [writes (:writes s)]
    (cond-> (-> pass
                (assoc-in [:w :chunks] (:chunks s))
                (out-into (edit/settled-deltas world s)))
      (:dirty pass)
      (update :dirty into (map (comp column first)) writes)
      (= :block-ticks k) (update :lit lit-writes writes))))

(defn- applied [world ctx k pass changes]
  (if (empty? changes)
    pass
    (let [op #(vector :set % (neighbors/flags-of % 3))
          ops (mapv op changes)]
      (written world k pass
               (neighbors/run (:chunks (:w pass)) ctx ops)))))

(defn- again-deltas [[p] {:keys [again]}]
  (when again
    [[:schedule-ticks {again [(chunk/block-pos->id p)]}]]))

(defn- stale? [pass [p] first-run]
  (or (nil? first-run)
      (touched? (:dirty pass) (:reach first-run) p)))

(defn- stepped
  [world ctx k pass [tick first-run]]
  (let [[pass r] (if (stale? pass tick first-run)
                   (rerun pass ctx k tick)
                   [pass first-run])
        pass (out-into pass (again-deltas tick r))]
    (applied world ctx k pass (:changes r))))

(defn- ordered [world k active]
  (let [runs? #(state/active-id? active %)
        pos (fn [[id ty]] [(chunk/id->block-pos id) ty])
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
    (first (reduce (fn [[flags seen] [p]]
                     [(conj! flags (not (crowded? seen size p)))
                      (conj! seen (bucket size p [0 0]))])
                   [(transient []) (transient (i/int-set))]
                   ticks))))

(defn- first-runs [world ctx k ticks]
  (let [r ((get-in lists [k :crowd]) ctx)
        lone (persistent! (lone-flags r ticks))
        spec (fn [i] [(when (lone i) (ran world ctx k (ticks i)))])]
    (if (some true? lone)
      (deltas/pmapcat spec (vec (range (count ticks))))
      (vec (repeat (count ticks) nil)))))

(defn- ticks-run [world k ticks]
  (let [ctx (state/level-ctx world)
        firsts (first-runs world ctx k ticks)
        w (update world :chunks chunk/editable)
        dirty (when (some some? firsts) (i/int-set))
        start {:w w :dirty dirty :lit {} :out (transient [])}]
    (persistent!
      (:out (reduce #(stepped world ctx k %1 %2) start
                    (map vector ticks firsts))))))

(defn- parked-ids [world active due]
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

(defn block-updates
  "Runs the block ticks that are due."
  [world _d]
  [#(ticks-deltas world :block-ticks)])

(defn fluid-updates [world _d]
  [#(ticks-deltas world :fluid-ticks)])

(defn- final-records [w recs]
  (let [at #(chunk/chunks-get-block (:chunks w) %)
        final (fn [pos] [pos (at pos)])]
    (into [] (comp (map first) (distinct) (map final)) recs)))

(defn- announced [w events]
  (let [heard (state/broadcast-chunks w)]
    (filter (fn [[cp _]] (contains? heard cp)) events)))

(defn- changed-out [w [cp recs]]
  (out/all (out/blocks-changed cp (final-records w recs))))

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
            (map #(changed-out w %) (announced w events))
            (entity-outs w events))))
