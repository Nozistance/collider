(ns collider.game.mob.armadillo
  "Armadillo scutes, shed and brushed off, and the end of its tick."
  (:require [collider.game.inventory :as inventory]
            [collider.game.mob.control :as control]
            [collider.game.mob.gift :as gift]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.shell :as shell]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:const scute-ticks 6000)

(def ^:private ^:const brush-wear 16)

(defn- scute-time ^long [t eid]
  (gift/wait t eid :scute-time scute-ticks))

(defn- shed-deltas [t eid e]
  (gift/gift-deltas t eid e :armadillo-shed :armadillo/scute-drop
                    :shed))

(defn- drops-loot? [world e]
  (and (not (mobs/baby? e)) (get-in world [:rules :mob-drops] true)))

(defn- timed [eid e at]
  [(assoc e :scute-at at) [[:merge-entity eid {:scute-at at}]]])

(defn shed
  "Returns living armadillo e after its scute time at tick t, with its
  deltas. When the time is up and it may drop loot it sheds a scute
  and waits again. The wait starts on its first tick and counts it."
  [world eid e t]
  (let [at (:scute-at e)]
    (cond
      (nil? at) (timed eid e (+ (long t) -1 (scute-time t eid)))
      (< (long t) (long at)) [e nil]
      (not (drops-loot? world e)) [e nil]
      :else (let [next-at (+ (long t) (scute-time t eid))
                  [e ds] (timed eid e next-at)]
              [e (concat (shed-deltas t eid e) ds)]))))

(defn- head-clamped
  "Returns scared armadillo e with its head turned to its body."
  [e]
  (let [hy (double (or (:head-yaw e) (:yaw e)))
        d (num/wrap-degrees (num/fsub (control/body-yaw e) hy))]
    (assoc e :head-yaw (num/f32 (+ hy d)))))

(defn- head-held [e]
  (if (shell/scared? e) (head-clamped e) e))

(defn ai-step
  "Returns armadillo e at the end of its tick, its head held to its
  body while scared, one tick longer in its state, and the delta
  that keeps the state."
  [eid e _]
  (let [e2 (shell/aged (head-held e))
        n (:in-state-ticks e2)
        m {:armadillo-state (shell/state e2) :in-state-ticks n}]
    (if (= n (:in-state-ticks e))
      [e2 nil]
      [e2 [[:merge-entity eid m]]])))

(defn- brushed [t peid p hand eid e]
  (let [pos (:pos e)]
    (concat (gift/dropped t eid e :armadillo-brush :brush)
            [(out/all (out/sound :armadillo/brush pos 1.0 1.0))]
            (signal/game-event :entity-interact pos eid)
            (inventory/hurt-item-deltas peid p hand brush-wear))))

(defn brush-result
  "Returns what a brush does to a grown armadillo. It brushes off a
  scute and wears the brush down."
  [{:keys [t peid p hand eid e item]}]
  (when (and (= :brush item) (not (mobs/baby? e)))
    {:result :success :deltas (brushed t peid p hand eid e)}))

(defn scared-result
  "Returns the answer of a scared armadillo to any other click, which
  is none."
  [{:keys [e]}]
  (when (shell/scared? e) {:result :pass}))
