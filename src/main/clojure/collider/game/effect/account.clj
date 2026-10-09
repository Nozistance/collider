(ns collider.game.effect.account
  "Mob effects on living entities, as they land, act and end.
  A change runs on a running account of one entity, its effects,
  the attributes they touched and the deltas so far."
  (:require [collider.game.attribute :as attribute]
            [collider.game.effect :as effect]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.food :as food]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]))

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
   :changed? false :hurt? false :sends? (not (player? e))})

(defn- own [acc msg]
  (if (player? (:e acc))
    (update acc :ds conj (out/to (:eid acc) msg))
    acc))

(defn- touched [acc k]
  (-> acc
      (update :dirty into (attribute/effect-attributes k))
      (update :e attribute/reclaimed k)))

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
        (update :ds conj [:merge-entity (:eid acc) m]))))

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
  "Returns the account after instance i of effect k lands.
  The account has :landed? true when it changed anything."
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
  "Returns the account without effect k.
  The account has :landed? true when it had k."
  [acc k]
  (if (contains? (:fx acc) k)
    (assoc (dropped acc k) :landed? true)
    (assoc acc :landed? false)))

(defn take-all
  "Returns the account without any effect.
  The account has :landed? true when it had one."
  [acc]
  (let [ks (map key (effect/in-order (:fx acc)))]
    (assoc (reduce dropped acc ks) :landed? (boolean (seq ks)))))

(defn- healed [acc ^double amount]
  (let [h (health acc)
        top (attribute/value (:e acc) (:fx acc) :max-health)]
    (if (pos? h)
      (set-health acc (min top (+ h amount)))
      acc)))

(def ^:private magic {:type :magic})

(def ^:private wither {:type :wither})

(defn- hurt [acc ^double amount src]
  (let [e (:e acc)
        n (hurt/taken (:world acc) e amount src)]
    (if (or (nil? n) (not (pos? (health acc))))
      acc
      (-> acc
          (assoc :e (entity/hurt e n) :hurt? true)
          (update :ds conj [:damage (:eid acc) n src])))))

(defn- regenerated [acc]
  (let [top (attribute/value (:e acc) (:fx acc) :max-health)]
    (if (< (health acc) top) (healed acc 1.0) acc)))

(defn- fed [acc ^long n]
  (if (player? (:e acc))
    (let [m (food/eaten (:e acc) n (* 2.0 n))]
      (-> acc
          (update :e merge m)
          (update :ds conj [:merge-entity (:eid acc) m])))
    acc))

(defn- acted
  "Returns [kept? account] after effect k at amplifier a acted.
  The effect goes when kept? is false."
  [acc k ^long a]
  (case k
    :regeneration [true (regenerated acc)]
    :poison [true (if (> (health acc) 1.0) (hurt acc 1.0 magic) acc)]
    :wither [true (hurt acc 1.0 wither)]
    :instant-health [true (healed acc (effect/heal-amount a))]
    :instant-damage [true (hurt acc (effect/harm-amount a) magic)]
    :saturation [true (fed acc (inc a))]
    :absorption [(pos? (double (:absorption (:e acc) 0.0))) acc]
    [true acc]))

(defn- event [acc k i ev]
  (case ev
    :refresh (-> acc (touched k)
                 (own (out/mob-effect (:eid acc) k i false))
                 refreshed)
    :updated (own acc (out/mob-effect (:eid acc) k i false))
    :gone (dropped acc k)))

(defn- acted-on [lived acc k i]
  (let [c (if (effect/endless? i) lived (long (:duration i)))
        a (long (:amplifier i))]
    (if (effect/due? k c a) (acted acc k a) [true acc])))

(defn- ticked [lived acc [k i]]
  (if-not (effect/remaining? i)
    (dropped acc k)
    (let [[kept? acc] (acted-on lived acc k i)
          [i' evs] (effect/stepped i)]
      (if kept?
        (reduce #(event %1 k i' %2) (put-fx acc k i') evs)
        (dropped acc k)))))

(defn- sent-deltas [eid e fx attrs]
  (let [es (attribute/entries e fx attrs)]
    (when (seq es)
      (cond-> [(out/all (out/attributes eid es))]
        (player? e) (conj (out/to eid (out/attributes eid es)))))))

(defn- attribute-deltas
  "Returns the deltas of the attributes the account touched. A mob
  sends them at once, a player keeps them for sync-deltas."
  [{:keys [eid e fx dirty sends?]}]
  (cond sends? (sent-deltas eid e fx dirty)
        (seq dirty)
        (let [ks (into (or (:dirty-attributes e) #{}) dirty)]
          [[:merge-entity eid {:dirty-attributes ks}]])))

(defn- absorption-change [acc e0]
  (let [a (:absorption (:e acc))]
    (when (not= a (:absorption e0)) {:absorption a})))

(defn- modifier-change [acc e0]
  (let [e (:e acc)
        ks [:equipment-modifiers :lost-modifiers]]
    (when-not (every? #(identical? (get e %) (get e0 %)) ks)
      (select-keys e ks))))

(defn- ambience-kept
  "The ambience the entity data keeps once the last effect is gone."
  [acc e0]
  (let [fx0 (:effects e0)]
    (when (and (:changed? acc) (empty? (:fx acc)) (seq fx0))
      {:ambience (effect/all-ambient? fx0)})))

(defn deltas
  "Returns the deltas of the account against entity e0 it began at."
  [acc e0]
  (let [acc (refreshed acc)
        m (merge (when (:changed? acc) {:effects (:fx acc)})
                 (absorption-change acc e0)
                 (modifier-change acc e0)
                 (ambience-kept acc e0))]
    (concat (when (seq m) [[:merge-entity (:eid acc) m]])
            (:ds acc)
            (attribute-deltas acc))))

(defn step
  "Returns the account after one tick of its effects.
  Its entity is lived ticks old."
  [acc lived]
  (reduce #(ticked (long lived) %1 %2)
          acc (effect/in-order (:fx acc))))

(defn- lived
  "Returns how many ticks entity e has lived in the world. A player
  counts the tick it joined in."
  ^long [world e]
  (cond-> (- (long (:tick world)) (long (or (:born e) 0)))
    (player? e) inc))

(defn- synced
  "Returns the account with the attributes entity e left to sync.
  The deltas that clear them come with it."
  [acc e]
  (if-let [ks (:dirty-attributes e)]
    (-> acc
        (update :dirty into ks)
        (update :ds conj
                [:merge-entity (:eid acc) {:dirty-attributes nil}]))
    acc))

(defn changed-deltas
  "Returns the deltas that follow a change of attributes attrs of
  entity eid, now e. The attributes go out, and health and absorption
  stay under their new top."
  [eid e attrs]
  (deltas (assoc (account eid e) :dirty (set attrs)) e))

(defn tick-deltas
  "Returns the deltas of one tick of the effects of entity eid.
  A mob sends the attributes left to sync with them, a player keeps
  them for sync-deltas."
  [world eid e]
  (when (or (seq (:effects e)) (:dirty-attributes e))
    (deltas (step (assoc (synced (account eid e) e) :world world)
                  (lived world e))
            e)))

(defn sync-deltas
  "Returns the deltas that send the attributes player eid, e, left to
  sync, all in one packet."
  [eid e]
  (when-let [ks (:dirty-attributes e)]
    (cons [:merge-entity eid {:dirty-attributes nil}]
          (sent-deltas eid e (:effects e) ks))))
