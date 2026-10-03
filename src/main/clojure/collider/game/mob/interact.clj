(ns collider.game.mob.interact
  "The answers of mobs to the clicks of players on them."
  (:require [collider.game.apply :as apply]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.mob.sheep :as sheep]
            [collider.game.mob.spec :as spec]
            [collider.game.mode :as game-mode]
            [collider.game.player :as player]
            [collider.game.reach :as reach]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private ^:const reach-buffer 3.0)

(defn- in-reach? [p e]
  (let [[half height] (mobs/box-of e)
        [ex ey ez] (reach/eye-pos p)
        [x y z] (:pos e)
        w (* 2.0 (double half))
        dx (reach/axis-gap ex (- (double x) (double half)) w)
        dy (reach/axis-gap ey (double y) (double height))
        dz (reach/axis-gap ez (- (double z) (double half)) w)
        r (+ (player/entity-reach p) reach-buffer)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- ctx-of
  [world t [_ peid target hand sneaking?]]
  (let [p (get-in world [:entities peid])
        e (get-in world [:entities target])
        hand (if (#{:off 1} hand) :off :main)]
    (when (and p e (mobs/mob-type? (:type e)))
      (let [p (assoc p :sneaking? (boolean sneaking?))]
        (when (in-reach? p e)
          {:world world :t t :peid peid :p p :eid target :e e
           :hand hand :item (sense/in-hand p hand)})))))

(defn- species-result [ctx]
  (some (fn [f] (f ctx)) (:results (spec/of (:type (:e ctx))))))

(def ^:private chain
  [(partial animal/egg-result spec/goals) species-result
   sheep/dye-result])

(defn- answered
  [{:keys [peid p e hand t]} {:keys [result deltas]}]
  (cond-> (vec deltas)
    (not= :pass result)
    (into (signal/game-event :entity-interact (:pos e) peid))
    (= :success-server result)
    (into (player/swing-deltas peid p hand t true))))

(defn- spectating? [world ev]
  (game-mode/spectator? (get-in world [:entities (nth ev 1)])))

(defn- interact [world ev t]
  (if-let [ctx (when-not (spectating? world ev) (ctx-of world t ev))]
    (answered ctx (or (some (fn [f] (f ctx)) chain) {:result :pass}))
    []))

(defn clicks
  "Returns the deltas of the answers of mobs to the clicks of players
  in events at tick t, one click after another."
  [world events t]
  (apply/fold-events world (filter #(= :interact (first %)) events)
                     (fn [w ev] (interact w ev t))))
