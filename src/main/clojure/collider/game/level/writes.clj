(ns collider.game.level.writes
  "How each delta writes the level. Blocks carry their light, block
  entities and ticks along, and entities, chunks and the weather have
  their own deltas."
  (:require [collider.cell :as cell]
            [collider.data.long-map :as lm]
            [collider.game.block.blockentity :as be]
            [collider.game.block.tickers :as tickers]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.orb :as orb]
            [collider.game.schedule :as schedule]
            [collider.game.schema :as schema]
            [collider.num :as num]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.weather :as weather]
            [collider.world.light :as light]
            [collider.world.neighbors :as neighbors]))

(set! *warn-on-reflection* true)

(defn- kind-changed? [old st]
  (and (be/kind old)
       (not= (block/block-of old) (block/block-of (long st)))
       (not= (be/kind old) (be/kind (long st)))))

(defn- openers-outside [openers id]
  (into {} (remove #(= id (chunk/block-chunk (key %)))) openers))

(defn- drop-block-entity [w pos]
  (-> w
      (update-in [:block-entities (chunk/block-chunk pos)] dissoc pos)
      (update :openers dissoc pos)
      (update :tickers tickers/without pos)))

(defn- reticked [w pos]
  (let [e (be/at w pos)
        st (chunk/at (:chunks w) pos)]
    (update w :tickers tickers/bound pos
            (and e (tickers/ticks? e st)))))

(defn- made-block-entity [w pos k]
  (if (be/at w pos)
    w
    (assoc-in w [:block-entities (chunk/block-chunk pos) pos]
              (be/made k (:tick w)))))

(defn- block-entity-changed [w [pos old st]]
  (let [k (be/kind st)]
    (cond-> w
      (kind-changed? old st) (drop-block-entity pos)
      k (made-block-entity pos k)
      (or k (be/kind old)) (reticked pos))))

(defn- block-entities-changed [w real]
  (reduce block-entity-changed w real))

(defn- real-changes [w changes]
  (chunk/changed (:chunks w) w changes))

(defn- add-changed [ev events]
  (reduce-kv (fn [ev k es] (update ev k #(if % (into % es) es)))
             (or ev (lm/long-map)) (chunk/by-chunk events)))

(defn- ticks-added [w ticks]
  (reduce-kv (fn [w k es]
               (update w k schedule/add-all
                       (mapv (fn [[_ at id ty]] [at id ty]) es)))
             w (group-by first ticks)))

(defn- unquiet [changes quiet]
  (if (seq quiet)
    (let [quiet (set quiet)]
      (filterv #(not (quiet (first %))) changes))
    changes))

(defn- with-changes [w real ticks quiet]
  (let [told (unquiet real quiet)]
    (cond-> (-> w
                (update :chunks chunk/editable)
                (update :chunks chunk/chunks-set-writes real)
                (update :chunks light/relight-batch real
                        (:sky? w true))
                (update :chunks chunk/frozen)
                (block-entities-changed real)
                (ticks-added ticks))
      (seq told) (update :changed-blocks add-changed told))))

(defn- settled
  ([w changes ticks] (settled w changes ticks nil))
  ([w changes ticks quiet]
   (let [real (real-changes w changes)]
     (if (and (empty? real) (empty? ticks))
       w
       (with-changes w real ticks quiet)))))

(defn- bare-records [s]
  (mapv (fn [[p st]] [p st]) (:records s)))

(defn- apply-set-blocks [w changes]
  (let [s (neighbors/set-blocks (:chunks w) (level/level-ctx w) changes)]
    (settled w (bare-records s) (:ticks s))))

(defn- drop-entities [es id]
  (reduce-kv (fn [es eid e]
               (if (schema/chunk-entity? id e) (dissoc es eid) es))
             es es))

(defn- born-yaw [spec t eid]
  (if (= :item (:type spec))
    (let [r (num/f32 (random/of-key t eid :item-yaw))]
      (assoc spec :yaw (num/f32 (* r 360.0))))
    spec))

(defn- spawned [w spec]
  (let [eid (long (:next-eid w schema/first-eid))
        t (:tick w)
        e (assoc (born-yaw spec t eid) :born t)]
    (-> w
        (assoc-in [:entities eid] (entity/of e))
        (assoc :next-eid (inc eid)))))

(defn- block-or-zero ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y)
    (chunk/at chunks p)
    0))

(defn- block-tick [w at id]
  (let [p (cell/unpack id)
        st (block-or-zero (:chunks w) p)]
    (update w :block-ticks schedule/add at id (block/block-of st))))

(defn set-blocks
  "Returns level w after the delta [:set-blocks changes ticks quiet].
  Without ticks the changes update their neighbours first."
  [w [_ changes ticks quiet]]
  (if ticks
    (settled w changes ticks quiet)
    (apply-set-blocks w changes)))

(defn spawn-entity
  "Returns level w after the delta [:spawn-entity spec]."
  [w [_ spec]]
  (spawned w spec))

(defn xp-award
  "Returns level w after the delta [:xp-award pos amount salt dir]."
  [w [_ pos amount salt dir]]
  (let [t (:tick w)]
    (orb/awarded w spawned pos (or dir [0.0 0.0 0.0])
                 (long amount) #(random/of-key t pos salt %))))

(defn schedule-ticks
  "Returns level w after the delta [:schedule-ticks at-ids]."
  [w [_ at-ids]]
  (reduce (fn [w [at ids]]
            (reduce #(block-tick %1 at %2) w ids))
          w at-ids))

(defn openers
  "Returns level w after the delta [:openers pos step]."
  [w [_ pos step]]
  (let [n (+ (long (get-in w [:openers pos] 0)) (long step))]
    (if (zero? n)
      (update w :openers dissoc pos)
      (assoc-in w [:openers pos] n))))

(defn shulker-anim
  "Returns level w after the delta [:shulker-anim pos a]."
  [w [_ pos a]]
  (if a
    (let [base {:progress (float 0.0)}]
      (update-in w [:shulker-anim pos] #(merge base % a)))
    (update w :shulker-anim dissoc pos)))

(defn add-chunk
  "Returns level w after the delta [:add-chunk id c]."
  [w [_ id c]]
  (if (contains? (:chunks w) id) w (update w :chunks assoc id c)))

(defn restore-chunk
  "Returns level w after the delta [:restore-chunk id payload]."
  [w [_ id payload]]
  (if (contains? (:chunks w) id)
    (update w :loading disj id)
    (let [[fresh n] (schema/refreshed w payload)]
      (schema/with-chunk (assoc w :next-eid n) id fresh))))

(defn- tickers-outside [w id]
  (reduce tickers/without (:tickers w)
          (keys (get-in w [:block-entities id]))))

(defn unload-chunk
  "Returns level w after the delta [:unload-chunk id]."
  [w [_ id]]
  (let [id (long id)]
    (-> w
        (assoc :tickers (tickers-outside w id))
        (update :chunks dissoc id)
        (update :block-entities dissoc id)
        (update :openers openers-outside id)
        (update :entities drop-entities id)
        (update :block-ticks schedule/dropped id)
        (update :fluid-ticks schedule/dropped id)
        (update :unknown dissoc id)
        (update :stored (fnil conj (lm/long-set)) id))))

(defn set-block-entity
  "Returns level w after the delta [:set-block-entity pos e]."
  [w [_ pos e]]
  (if (and e (be/kind (chunk/at (:chunks w) pos)))
    (-> w
        (assoc-in [:block-entities (chunk/block-chunk pos) pos] e)
        (reticked pos))
    (drop-block-entity w pos)))

(defn advance-weather
  "Returns level w after the delta [:advance-weather m].
  Without m the weather takes its own next step."
  [w [_ m]]
  (reduce-kv (fn [w k v] (if (= v (get w k)) w (assoc w k v)))
             w (or m (weather/advance w))))
