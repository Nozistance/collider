(ns collider.game.game-mode
  "Game modes of players and the abilities each gives."
  (:require [collider.config :as config]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ids
  "GameType ids of the game modes."
  {:survival 0 :creative 1 :adventure 2 :spectator 3})

(def ^:private by-id
  (into {} (map (fn [[k v]] [v k])) ids))

(def ^:private by-name
  (into {} (map (fn [k] [(name k) k])) (keys ids)))

(defn of-id
  "Returns the mode of GameType id n: survival for an unknown id."
  [n]
  (get by-id n :survival))

(defn named
  "Returns the mode GameType.byName finds for s, or nil."
  [s]
  (by-name s))

(defn id
  "Returns the id of mode, -1 for no mode."
  ^long [mode]
  (long (get ids mode -1)))

(defn default-mode
  "Returns the mode a player without a save joins in."
  [world]
  (get-in world [:config :game-mode] (:game-mode config/defaults)))

(defn forced-mode
  "DedicatedServer.getForcedGameType: the default mode when
  force-game-mode is on, else nil."
  [world]
  (when (get-in world [:config :force-game-mode])
    (default-mode world)))

(defn joining-mode
  "ServerPlayer.calculateGameModeForNewPlayer for the saved mode."
  [world saved]
  (or (forced-mode world) saved (default-mode world)))

(defn creative? [e] (= :creative (:game-mode e)))

(defn spectator? [e] (= :spectator (:game-mode e)))

(defn may-fly?
  "Returns true when the mode of player e lets it fly."
  [e]
  (contains? #{:creative :spectator} (:game-mode e)))

(defn flying-in
  "GameType.updatePlayerAbilities: whether a player that flies as
  flying? does in mode."
  [mode flying?]
  (case mode
    :creative (boolean flying?)
    :spectator true
    false))

(defn abilities
  "Returns the abilities of player e as its client learns them."
  [e]
  {:invulnerable? (may-fly? e) :flying? (boolean (:flying e))
   :may-fly? (may-fly? e) :instabuild? (creative? e)})

(def ^:const block-range 4.5)

(def ^:const creative-block-range 0.5)

(def ^:const entity-range 3.0)

(def ^:const creative-entity-range 2.0)

(defn reach-attributes
  "ServerPlayer.updatePlayerAttributes: the interaction ranges of
  player e, the creative_mode modifiers only in creative."
  [e]
  (let [c? (creative? e)
        mod (fn [k r] (if c? [[k r 0]] []))]
    [[:entity-interaction-range entity-range
      (mod :creative-mode-entity-range creative-entity-range)]
     [:block-interaction-range block-range
      (mod :creative-mode-block-range creative-block-range)]]))

(defn- in-range-of-ground?
  "ServerPlayerGameMode.isInRangeOfGround: no block in the body of
  e, and one within a block below it."
  [chunks e]
  (let [[half h] (entity/pose-box (:pose e :standing))
        pos (:pos e)]
    (and (phys/free? chunks pos half h 0.0 0.0 0.0)
         (not (phys/free? chunks pos half 1.0 0.0 -1.0 0.0)))))

(defn- flying-after [chunks e mode]
  (let [f (flying-in mode (:flying e))]
    (and f (or (= :spectator mode)
               (not (in-range-of-ground? chunks e))))))

(defn- changes [chunks e mode]
  (cond-> {:game-mode mode :previous-game-mode (:game-mode e)
           :flying (flying-after chunks e mode)}
    (= :spectator mode) (assoc :using-item? false :using nil)))

(defn- reach-deltas [eid e e']
  (when (not= (creative? e) (creative? e'))
    (let [attrs (reach-attributes e')]
      [(out/to eid (out/attributes eid attrs))
       (out/all (out/attributes eid attrs))])))

(defn change
  "ServerPlayer.setGameMode: returns the deltas that put player e of
  id eid in mode, nil when it is in mode already."
  [world eid e mode]
  (when (not= mode (:game-mode e))
    (let [m (changes (:chunks world) e mode)
          e' (merge e m)
          able (out/abilities (abilities e'))]
      (concat
        [[:merge-entity eid m]
         (out/to eid able)
         (out/everyone (out/tab-game-mode (:uuid e) mode))
         (out/to eid (out/game-mode mode))
         (out/to eid able)]
        (reach-deltas eid e e')))))
