(ns collider.game.areas
  "The chunk areas the players of a level keep loaded and ticking."
  (:require [clojure.data.int-map :as i]
            [collider.game.mode :as game-mode]
            [collider.game.level :as level]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private activation-radius 2)

(defn view-radius
  "Returns the view distance in chunks the config asks for.
  It stays between 2 and 32."
  ^long [world]
  (-> (long (get-in world [:config :view-distance] 7))
      (max 2)
      (min 32)))

(defn- loads-chunks? [world e]
  (and (= :player (:type e))
       (or (get-in world [:rules :spectators-generate-chunks] true)
           (not (game-mode/spectator? e)))))

(defn player-chunks
  "Returns the ids of the chunks that the players who load chunks
  stand in."
  [world]
  (let [es (:entities world)
        at (fn [eid]
             (when-let [e (get es eid)]
               (when (loads-chunks? world e)
                 (chunk/pos-chunk (:pos e)))))]
    (into (i/int-set) (keep at) (vals (:players world)))))

(defn- zone-at [ids ^long r]
  (into (i/int-set)
        (mapcat (fn [id]
                  (let [[cx cz] (chunk/id->pos id)]
                    (chunk/around-ids (long cx) (long cz) r))))
        ids))

(defn- sim-radius ^long [world]
  (long (get-in world [:config :simulation-distance]
                activation-radius)))

(defn- compute-areas [world]
  (let [ps (player-chunks world)
        zone (zone-at ps (+ 2 (view-radius world)))
        s (sim-radius world)
        live? #(and (contains? zone %)
                    (contains? (:chunks world) %))
        in? #(into (i/int-set) (filter live?) %)
        absent (into (i/int-set)
                     (remove #(contains? (:chunks world) %)) zone)
        active (in? (zone-at ps s))
        broadcast (zone-at ps (inc (view-radius world)))]
    {:active active :ticking (in? (zone-at ps (inc s)))
     :broadcast broadcast :zone zone :absent absent
     :active-ids (vec active)}))

(defn- areas-key
  "Returns what the chunk areas of world are a function of.
  Block writes keep the shape of chunks, so they do not count."
  [world]
  {:player-chunks (player-chunks world)
   :shape (chunk/shape (:chunks world))
   :config (:config world) :rules (:rules world)
   :players (:players world) :entities (:entities world)})

(defn- same-shape? [k world]
  (and (identical? (:shape k) (chunk/shape (:chunks world)))
       (identical? (:config k) (:config world))
       (identical? (:rules k) (:rules world))))

(defn- same? [k world]
  (and (identical? (:entities k) (:entities world))
       (identical? (:players k) (:players world))
       (same-shape? k world)))

(defn- reusable? [k world ps]
  (and (same-shape? k world) (= (:player-chunks k) ps)))

(defn- fresh? [world had]
  (when had
    (let [k (::key (meta had))]
      (or (same? k world)
          (reusable? k world (player-chunks world))))))

(defn- areas [world]
  (let [had (:active-chunks world)]
    (if (fresh? world had) had (compute-areas world))))

(defn loaded-zone
  "Returns the ids of the chunks the players keep at full status.
  That zone reaches two rings past the view distance."
  [world]
  (:zone (areas world)))

(defn absent-chunks
  "Returns the ids of the loaded zone that world holds no chunk for."
  [world]
  (:absent (areas world)))

(defn active-chunks
  "Returns the chunks that run entity and random ticks."
  [world]
  (:active (areas world)))

(defn active-chunk-ids
  "Returns the active chunks as a vector, in the order of the set."
  [world]
  (:active-ids (areas world)))

(defn ticking-chunks
  "Returns the chunks that run scheduled block and fluid ticks.
  They reach one chunk further than the entity ticking ones."
  [world]
  (:ticking (areas world)))

(defn broadcast-chunks
  "Returns the chunks whose block changes reach the clients.
  They reach one ring past the view distance."
  [world]
  (:broadcast (areas world)))

(defn- kept [world had now]
  (let [k (::key (meta had))]
    (if (and had (reusable? k world (:player-chunks now)))
      had
      (compute-areas world))))

(defn cache-active-chunks
  "Returns the world with its chunk areas up to date.
  The areas follow its players and chunks. The key they were made
  from is metadata, so equal worlds stay equal."
  [world]
  (let [had (:active-chunks world)]
    (if (and had (same? (::key (meta had)) world))
      world
      (let [now (areas-key world)]
        (assoc world :active-chunks
               (with-meta (kept world had now) {::key now}))))))

(defn active-at?
  "Returns true when the chunk of block pos is in active."
  [active pos]
  (contains? active (chunk/pos-chunk pos)))

(defn active-id?
  "Returns true when the chunk of block id bid is in active."
  [active ^long bid]
  (contains? active (chunk/block-id-chunk bid)))

(defn active-of-types
  "Returns the entries of type ts whose entity is in an active chunk."
  [world ts]
  (let [active (active-chunks world)
        live? (fn [[_ e]] (active-at? active (:pos e)))]
    (into [] (filter live?) (level/of-types world ts))))
