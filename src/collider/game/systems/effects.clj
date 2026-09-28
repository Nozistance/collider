(ns collider.game.systems.effects
  "Mob effects on living entities, as they land, act and end.
  A change runs on a running account of one entity, its effects,
  the attributes they touched and the deltas so far."
  (:require [collider.game.attribute :as attribute]
            [collider.game.effect :as effect]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
            [collider.game.systems.damage :as damage]))

(set! *warn-on-reflection* true)

(defn- player? [e] (= :player (:type e)))

(defn living?
  "Returns true when entity e can have effects."
  [e]
  (or (player? e) (mobs/mob-type? (:type e))))

(defn account
  "Returns the account of entity e with id eid before any change."
  [eid e]
  {:eid eid :e e :fx (or (:effects e) {}) :ds [] :dirty #{}
   :changed? false :hurt? false})

(defn- own [acc msg]
  (if (player? (:e acc))
    (update acc :ds conj (out/to (:eid acc) msg))
    acc))

(defn- touched [acc k]
  (update acc :dirty into (attribute/effect-attributes k)))

(defn- put-fx [acc k i]
  (-> acc
      (assoc :fx (assoc (:fx acc) k i) :changed? true)
      (assoc-in [:e :effects] (assoc (:fx acc) k i))))

(defn- max-absorption ^double [acc]
  (attribute/value (:e acc) (:fx acc) :max-absorption))

(defn- absorbed [acc k i]
  (if (= :absorption k)
    (let [a (double (:absorption (:e acc) 0.0))
          want (double (* 4 (inc (long (:amplifier i)))))
          v (min (max a want) (max-absorption acc))]
      (assoc-in acc [:e :absorption] (double (float v))))
    acc))

(defn- health ^double [acc] (double (:health (:e acc) 0.0)))

(defn- set-health [acc ^double h]
  (let [h (double (float h))
        m (cond-> {:health h}
            (not (:hurt? acc)) (assoc :health-sent h))]
    (-> acc
        (assoc-in [:e :health] h)
        (update :ds conj [:merge-entity (:eid acc) m])
        (own (out/health h)))))

(defn- clamp-health [acc]
  (let [top (attribute/value (:e acc) (:fx acc) :max-health)]
    (if (> (health acc) top)
      (set-health acc top)
      acc)))

(defn- clamp-absorption [acc]
  (let [a (double (:absorption (:e acc) 0.0))
        top (max-absorption acc)]
    (if (> a top)
      (assoc-in acc [:e :absorption] (double (float top)))
      acc)))

(defn- refreshed [acc] (clamp-absorption (clamp-health acc)))

(defn land
  "Returns the account after instance i of effect k lands, and
  :landed? true when it changed anything. This is addEffect with
  onEffectAdded, onEffectUpdated and onEffectStarted."
  [acc k i]
  (let [eid (:eid acc)
        [fx what] (effect/added (:fx acc) k i)
        acc (cond-> (put-fx acc k (fx k)) what (touched k))
        shown #(own acc (out/mob-effect eid k (fx k) %))
        acc (case what
              :added (shown true)
              :updated (refreshed (shown false))
              acc)]
    (assoc (absorbed acc k i) :landed? (some? what))))

(defn- dropped [acc k]
  (-> acc
      (assoc :fx (dissoc (:fx acc) k) :changed? true)
      (assoc-in [:e :effects] (dissoc (:fx acc) k))
      (touched k)
      (own (out/mob-effect-gone (:eid acc) k))
      refreshed))

(defn take-off
  "Returns the account without effect k, and :landed? true when it
  had it. This is removeEffect."
  [acc k]
  (if (contains? (:fx acc) k)
    (assoc (dropped acc k) :landed? true)
    (assoc acc :landed? false)))

(defn take-all
  "Returns the account without any effect, and :landed? true when
  it had one. This is removeAllEffects."
  [acc]
  (let [ks (map key (effect/in-order (:fx acc)))]
    (assoc (reduce dropped acc ks) :landed? (boolean (seq ks)))))

(defn- healed [acc ^double amount]
  (let [h (health acc)
        top (attribute/value (:e acc) (:fx acc) :max-health)]
    (if (pos? h)
      (set-health acc (min top (+ h amount)))
      acc)))

(defn- hurt [acc ^double amount]
  (let [e (:e acc)]
    (if (or (damage/creative-proof? e) (not (pos? (health acc))))
      acc
      (-> acc
          (assoc :e (state/hurt e amount) :hurt? true)
          (update :ds conj [:damage (:eid acc) amount])))))

(defn- regenerated [acc]
  (let [top (attribute/value (:e acc) (:fx acc) :max-health)]
    (if (< (health acc) top) (healed acc 1.0) acc)))

(defn- acted
  "Returns [kept? account] after effect k at amplifier a acted.
  This is MobEffect.applyEffectTick of each kind."
  [acc k ^long a]
  (case k
    :regeneration [true (regenerated acc)]
    :poison [true (if (> (health acc) 1.0) (hurt acc 1.0) acc)]
    :wither [true (hurt acc 1.0)]
    :instant-health [true (healed acc (effect/heal-amount a))]
    :instant-damage [true (hurt acc (effect/harm-amount a))]
    :absorption [(pos? (double (:absorption (:e acc) 0.0))) acc]
    [true acc]))

(defn- event [acc k i ev]
  (case ev
    :refresh (-> acc (touched k)
                 (own (out/mob-effect (:eid acc) k i false))
                 refreshed)
    :updated (own acc (out/mob-effect (:eid acc) k i false))
    :gone (dropped acc k)))

(defn- acted-on [tick acc k i]
  (let [c (if (effect/endless? i) tick (long (:duration i)))
        a (long (:amplifier i))]
    (if (effect/due? k c a) (acted acc k a) [true acc])))

(defn- ticked [tick acc [k i]]
  (if-not (effect/remaining? i)
    (dropped acc k)
    (let [[kept? acc] (acted-on tick acc k i)
          [i' evs] (effect/stepped i)]
      (if kept?
        (reduce #(event %1 k i' %2) (put-fx acc k i') evs)
        (dropped acc k)))))

(defn- attribute-deltas [{:keys [eid e fx dirty]}]
  (let [es (attribute/entries e fx dirty)]
    (when (seq es)
      (cond-> [(out/all (out/attributes eid es))]
        (player? e) (conj (out/to eid (out/attributes eid es)))))))

(defn- absorption-change [acc e0]
  (let [a (:absorption (:e acc))]
    (when (not= a (:absorption e0)) {:absorption a})))

(defn deltas
  "Returns the deltas of the account against entity e0 it began at."
  [acc e0]
  (let [acc (refreshed acc)
        m (merge (when (:changed? acc) {:effects (:fx acc)})
                 (absorption-change acc e0))]
    (concat (when (seq m) [[:merge-entity (:eid acc) m]])
            (:ds acc)
            (attribute-deltas acc))))

(defn step
  "Returns the account after one tick of its effects at game tick
  tick. This is LivingEntity.tickEffects."
  [acc tick]
  (reduce #(ticked (long tick) %1 %2)
          acc (effect/in-order (:fx acc))))

(defn- entity-deltas [world [eid e]]
  (deltas (step (account eid e) (:tick world)) e))

(defn- affected [world]
  (filter (fn [[_ e]] (and (seq (:effects e)) (living? e)))
          (:entities world)))

(defn effects
  "Returns a step that ticks the effects of every living entity."
  [world _]
  [#(into [] (mapcat (fn [x] (entity-deltas world x)))
          (affected world))])
