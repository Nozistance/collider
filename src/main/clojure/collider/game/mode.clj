(ns collider.game.mode
  "Game modes of players and the abilities each gives."
  (:require [collider.game.attribute :as attribute]
            [collider.game.entity :as entity]
            [collider.game.out :as out]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ids
  {:survival 0 :creative 1 :adventure 2 :spectator 3})

(def ^:private by-id
  (into {} (map (fn [[k v]] [v k])) ids))

(def ^:private by-name
  (into {} (map (fn [k] [(name k) k])) (keys ids)))

(defn of-id
  "Returns the mode of id n, or survival for an unknown id."
  [n]
  (get by-id n :survival))

(defn named
  "Returns the mode that s names, or nil."
  [s]
  (by-name s))

(defn id
  "Returns the id of mode, -1 for no mode."
  ^long [mode]
  (long (get ids mode -1)))

(defn default-mode
  "Returns the mode a player without a save joins in."
  [world]
  (get-in world [:config :game-mode] :creative))

(defn forced-mode
  "Returns the default mode when force-game-mode is on, else nil."
  [world]
  (when (get-in world [:config :force-game-mode])
    (default-mode world)))

(defn joining-mode
  "Returns the mode a player joins in, given its saved mode."
  [world saved]
  (or (forced-mode world) saved (default-mode world)))

(defn creative? [e] (= :creative (:game-mode e)))

(defn spectator? [e] (= :spectator (:game-mode e)))


(defn- held? [chunks cx cz]
  (contains? chunks (chunk/pos->id cx cz)))

(defn- cells [^double lo ^double hi]
  (range (bit-shift-right (long (Math/floor lo)) 4)
         (inc (bit-shift-right (long (Math/ceil hi)) 4))))

(defn- touching-unloaded? [chunks e]
  (let [half (first (entity/pose-box (:pose e :standing)))
        r (+ 1.0 (double half))
        x (v/x (:pos e)) z (v/z (:pos e))]
    (boolean
      (some (fn [cx]
              (some #(not (held? chunks cx %))
                    (cells (- z r) (+ z r))))
            (cells (- x r) (+ x r))))))

(defn ticks?
  "Returns true when player e runs its player tick.
  A spectator that touches an unloaded chunk does not."
  [chunks e]
  (not (and (spectator? e) (touching-unloaded? chunks e))))

(defn shown-to?
  "Returns true when viewer tracks entity e.
  A spectator sees the players that look through their own eyes. The
  others see no spectator."
  [viewer e]
  (or (not= :player (:type e))
      (if (spectator? viewer)
        (nil? (:camera e))
        (not (spectator? e)))))

(defn seen?
  "Returns true when e is alive and no spectator.
  Mobs look at, follow and target only such an entity."
  [e]
  (and (not (spectator? e)) (pos? (double (:health e 1.0)))))

(def ^:private mode-abilities
  {:survival {:may-build? true}
   :creative {:invulnerable? true :may-fly? true :instabuild? true
              :may-build? true}
   :adventure {}
   :spectator {:invulnerable? true :may-fly? true :flies? true}})

(defn- able? [e k]
  (boolean (get-in mode-abilities [(:game-mode e) k])))

(defn invulnerable?
  "Returns true when the mode of player e keeps it from harm."
  [e]
  (able? e :invulnerable?))

(defn may-build?
  "Returns true when the mode of player e lets it build."
  [e]
  (able? e :may-build?))

(defn may-fly?
  "Returns true when the mode of player e lets it fly."
  [e]
  (able? e :may-fly?))

(defn instabuild?
  "Returns true when the mode of player e breaks blocks at once and
  uses items without using them up."
  [e]
  (able? e :instabuild?))

(defn flying-in
  "Returns true when a player with flying? still flies in mode."
  [mode flying?]
  (let [{:keys [flies? may-fly?]} (mode-abilities mode)]
    (boolean (or flies? (and may-fly? flying?)))))

(defn abilities
  "Returns the abilities of player e as its client learns them."
  [e]
  {:invulnerable? (invulnerable? e) :flying? (boolean (:flying e))
   :may-fly? (may-fly? e) :instabuild? (instabuild? e)})

(def ^:const entity-range 3.0)

(defn reach-attributes
  "Returns the interaction ranges of player e.
  The creative modifiers apply only in creative."
  [e]
  (mapcat #(attribute/entries e (:effects e) #{%})
          [:entity-interaction-range :block-interaction-range]))

(defn- in-range-of-ground? [chunks e]
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

(def ^:private reaches
  #{:entity-interaction-range :block-interaction-range})

(defn- reach-deltas
  "Returns the delta that leaves the interaction ranges of player eid
  to sync when its mode changes them."
  [eid e e']
  (when (not= (creative? e) (creative? e'))
    (let [ks (into (or (:dirty-attributes e) #{}) reaches)]
      [[:merge-entity eid {:dirty-attributes ks}]])))

(defn change
  "Returns the deltas that put player e of id eid in mode, or nil when
  it is in mode already."
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
