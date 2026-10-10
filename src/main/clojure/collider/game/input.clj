(ns collider.game.input
  "The events of a tick the world heeds, and what they did to
  their players."
  (:require [collider.game.player :as player]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]))

(set! *warn-on-reflection* true)

(def ^:private move-keys
  [:on-ground :sprinting? :pose :swimming? :eye-in-water? :in-water?
   :landed])

(defn- jump? [e e']
  (and (:on-ground e) (not (:on-ground e'))
       (> (v/y (:pos e')) (v/y (:pos e)))))

(defn- move-of
  "Returns what a :move event did to its player, or nil when it moved
  none. A player who sleeps or awaits a teleport does not move."
  [w w' [tag eid changes]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) (:pos changes) (:pos e) e'
               (not (:tp-target e)) (not (:sleeping e)))
      (assoc (select-keys e' move-keys)
             :eid eid :from (:pos e) :to (:pos e')
             :jump? (jump? e e')
             :climbing?
             (climb/on-climbable? (:chunks w') (:pos e'))))))

(defn- quit-of
  [w [tag eid]]
  (when (= :player-quit tag)
    (when-let [e (get-in w [:entities eid])]
      (assoc e :eid eid))))

(defn- resend-of
  [w w' [tag eid]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) e' (not= (:tp-id e) (:tp-id e')))
      {:eid eid :pos (:tp-target e)
       :yaw (:yaw e) :pitch (:pitch e)})))

(defn- release-of
  "Returns the use a release event lets go of, with its player.
  Any other event, or an item no longer in hand, gives nil."
  [w [tag eid]]
  (when (= :release-use tag)
    (let [e (get-in w [:entities eid])
          {:keys [hand item] :as u} (:using e)]
      (when (and u (= item (:item (player/hand-stack e hand))))
        (assoc u :eid eid :pos (:pos e))))))

(def ^:private load-gated
  "The events that count only from a client that has loaded."
  #{:move :input :dig :release-use :place :use-item :entity-action
    :attack :interact :spectate})

(defn heeded?
  "Returns true when the world takes event d into account."
  [w [tag eid]]
  (or (not (contains? load-gated tag))
      (when-let [e (get-in w [:entities eid])]
        (player/client-loaded? e (long (:tick w))))))

(defn heard
  "Returns input record acc with what event d did to w, giving w'."
  [acc w w' d]
  (let [o (player/use-origin w d) m (move-of w w' d) q (quit-of w d)
        r (resend-of w w' d) u (release-of w d)]
    (cond-> (update acc :heeded conj d)
      o (assoc-in [:use-origins (count (:heeded acc))] o)
      m (update :moves conj m)
      q (update :quits conj q)
      r (update :resends conj r)
      u (update :releases conj u))))

(def unheard
  "The input record of a tick before its first event."
  {:heeded [] :use-origins {} :moves [] :quits [] :resends []
   :releases []})

(defn as-came
  "Returns world with the player of event ev, the i-th of the tick,
  as it was when the event came."
  [world i ev]
  (let [eid (nth ev 1)
        origin (get-in world [:input :use-origins i])
        e (get-in world [:entities eid])]
    (if (and origin e)
      (assoc-in world [:entities eid] (merge e origin))
      world)))
