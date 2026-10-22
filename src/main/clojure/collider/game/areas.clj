(ns collider.game.areas
  "The chunk areas the players of a level keep loaded and ticking."
  (:require [collider.data.long-map :as lm]
            [collider.game.entity :as entity]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.vec :as v]
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
       (not (entity/removed? e))
       (or (get-in world [:rules :spectators-generate-chunks] true)
           (not (game-mode/spectator? e)))))

(defn player-chunks
  "Returns the ids of the chunks that the players who load
  chunks stand in."
  [world]
  (let [es (:entities world)
        at (fn [eid]
             (when-let [e (get es eid)]
               (when (loads-chunks? world e)
                 (chunk/pos-chunk (:pos e)))))]
    (into (lm/long-set) (keep at) (vals (:players world)))))

(defn- zone-at [ids ^long r]
  (into (lm/long-set)
        (mapcat (fn [id]
                  (let [[cx cz] (chunk/id->pos id)]
                    (chunk/around-ids (long cx) (long cz) r))))
        ids))

(defn- sim-radius ^long [world]
  (long (get-in world [:config :simulation-distance]
                activation-radius)))

(defn- inputs
  "Returns what the chunk areas of world are a function of."
  [world]
  [(player-chunks world) (chunk/shape (:chunks world))
   (view-radius world) (sim-radius world)])

(defn- computed [world [ps _ r s :as of]]
  (let [r (long r) s (long s)
        zone (zone-at ps (+ 2 r))
        live? #(and (contains? zone %)
                    (contains? (:chunks world) %))
        live-of #(into (lm/long-set) (filter live?) %)
        absent (into (lm/long-set)
                     (remove #(contains? (:chunks world) %)) zone)
        active (live-of (zone-at ps s))]
    (with-meta {:active active :ticking (live-of (zone-at ps (inc s)))
                :broadcast (zone-at ps (inc r)) :zone zone :absent absent
                :active-ids (vec active)}
               {::of of})))

(defn- areas [world]
  (let [had (:active-chunks world) of (inputs world)]
    (if (= of (::of (meta had))) had (computed world of))))

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
  "Returns the ids of the active chunks in the order of their set."
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

(defn with-areas
  "Returns the world with its chunk areas up to date.
  The areas follow its players and chunks."
  [world]
  (let [had (:active-chunks world) now (areas world)]
    (if (identical? had now) world (assoc world :active-chunks now))))

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

(defn awaited-ids
  "Returns the ids of the chunks of level world that joining and
  respawning players wait for."
  [world]
  (let [dim (:dim world :overworld)]
    (mapcat #(get (:need %) dim) (vals (:spawning world)))))

(defn needed-ids
  "Returns the ids of the chunks the world keeps loaded.
  They are the chunks around its players and the chunks joining and
  respawning players wait for."
  [world]
  (into (loaded-zone world) (awaited-ids world)))

(defn- within? [radius [px py pz] [cx cy cz]]
  (let [dx (- (double px) (double cx))
        dy (- (double py) (double cy))
        dz (- (double pz) (double cz))]
    (< (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
       (double radius))))

(def ^:private ^:const everywhere -1)

(defn- player-near? [world radius c]
  (some (fn [eid]
          (when-let [q (get-in world [:entities eid :pos])]
            (within? radius q c)))
        (vals (:players world))))

(defn fire-spreads-at?
  "Returns true when fire may catch and spread at block p of level
  world, near enough to a player or with no limit."
  [world p]
  (let [rule [:rules :fire-spread-radius-around-player]
        radius (long (get-in world rule 128))]
    (or (= radius everywhere)
        (player-near? world radius (v/centre p)))))
