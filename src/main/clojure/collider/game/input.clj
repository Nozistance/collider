(ns collider.game.input
  "The events of a tick the world heeds, and what they did
  to their players."
  (:require [collider.game.player :as player]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]))

(set! *warn-on-reflection* true)

(def ^:private move-keys
  [:on-ground :sprinting? :pose :swimming? :eye-in-water? :in-water?
   :landed :fall-flying])

(defn- jump? [e e']
  (and (:on-ground e) (not (:on-ground e'))
       (> (v/y (:pos e')) (v/y (:pos e)))))

(defn- move-of
  "Returns what a :move event did to its player, or nil when it moved
  none. A player who sleeps or awaits a teleport does not move."
  [w w' [tag eid changes]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) (:pos changes) (:pos e) e'
               (not (:tp-target e)) (not (:sleeping e))
               (not (:vehicle e)))
      (assoc (select-keys e' move-keys)
             :eid eid :from (:pos e) :to (:pos e')
             :jump? (jump? e e')
             :climbing?
             (climb/climbing? (:chunks w') e')))))

(defn moves-of
  "Returns the moves [from to d] the packets of player eid made in the
  tick of world."
  [world eid]
  (for [{f :from t :to :as m} (get-in world [:input :moves])
        :when (= eid (:eid m))]
    [f t (mapv - t f)]))

(defn- quit-of
  [w [tag eid]]
  (when (= :player-quit tag)
    (when-let [e (get-in w [:entities eid])]
      (assoc e :eid eid))))

(defn- resend-of
  [w w' [tag eid changes]]
  (let [e (get-in w [:entities eid]) e' (get-in w' [:entities eid])]
    (when (and (= :move tag) e' (not= (:tp-id e) (:tp-id e')))
      (let [[y x] (if (:tp-target e)
                    [(:yaw e) (:pitch e)]
                    (player/move-turn e changes))]
        {:eid eid :pos (:tp-target e') :yaw y :pitch x}))))

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
  "Returns input record acc with event d heard in w."
  [acc w d]
  (let [q (quit-of w d)]
    (cond-> (update acc :heeded conj d)
      q (update :quits conj q))))

(defn played
  "Returns what packet event d adds to the input record, w' being w
  after it. That is its move, its resent teleport and the use it let
  go, each only when there is one."
  [w w' d]
  (let [m (move-of w w' d) r (resend-of w w' d) u (release-of w d)]
    (cond-> {}
      m (assoc :moves m)
      r (assoc :resends r)
      u (assoc :releases u))))

(def unheard
  "The input record of a tick before its first event."
  {:heeded [] :moves [] :quits [] :resends [] :releases []})
