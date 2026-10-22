(ns collider.game.mob.push
  "Shoves between overlapping bodies."
  (:require [collider.game.entity.hurt :as hurt]
            [collider.game.entity.sections :as sections]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.team :as team]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]
            [collider.world.chunk :as chunk])
  (:import (clojure.lang MapEntry)
           (collider.game.mob Bodies PushGrid Turns)))

(set! *warn-on-reflection* true)

(def ^:private no-box [0.0 1.0])

(defn- pushable-box [e]
  (or (size/box e) no-box))

(defn- pushable-half ^double [e]
  (double (nth (pushable-box e) 0)))

(defn- pushable-height ^double [e]
  (double (nth (pushable-box e) 1)))

(defn alive?
  "Returns true when body e has health above zero or keeps none. A
  dead body takes no shoves but still steps and shoves the living."
  [e]
  (let [h (:health e)] (or (nil? h) (pos? (double h)))))

(defn body?
  "Returns true when the entity of entry stands in a chunk of held
  and joins the shoves. A spectator never joins, and a mob leaves
  when its death ends."
  [held [_ e]]
  (and (or (= :player (:type e))
           (and (mobs/mob-type? (:type e))
                (not (mobs/death-ends? e))))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn pushable?
  "Returns true when body e takes shoves. A dead body, an aloof kind
  and a body that climbs in chunks take none. The climber still
  shoves the others."
  [chunks e]
  (and (alive? e) (not (mobs/flag? e :aloof?))
       (not (climb/climbing? chunks e))))

(defn- came ^long [e]
  (if-let [a (:arrived e)] (long (nth a 0)) Long/MIN_VALUE))

(defn- rank ^long [eid e]
  (if-let [r (nth (:arrived e) 1 nil)] (long r) (* 2 (long eid))))

(defn- by-id? [es]
  (and (vector? es)
       (loop [i 1]
         (or (>= i (count es))
             (and (< (long (nth (nth es (dec i)) 0))
                     (long (nth (nth es i) 0)))
                  (recur (inc i)))))))

(defn moved
  "Moves body eid of index to the place and box of e
  and returns index."
  [^PushGrid index eid e]
  (let [p (:pos e) [h t] (pushable-box e)]
    (PushGrid/moved index (long eid) (double h) (double t)
                    (v/x p) (v/y p) (v/z p) (came e) (rank eid e))))

(defn- same-section? [c c']
  (== (bit-shift-right (long (Math/floor (double c))) 4)
      (bit-shift-right (long (Math/floor (double c'))) 4)))

(defn arrived
  "Returns when body eid came into its entity section once e moves
  to to in tick t. Bodies of one section meet in the order they came.
  A body that stays in its section keeps its arrival, and one that
  leaves it arrives in tick t after the bodies of lower id."
  [e to t eid]
  (let [from (:pos e)]
    (if (and (same-section? (v/x from) (v/x to))
             (same-section? (v/y from) (v/y to))
             (same-section? (v/z from) (v/z to)))
      (:arrived e)
      [(* 2 (long t)) (inc (* 2 (long eid)))])))

(defn slot
  "Returns the slot of body eid in index, its place among the bodies
  by id, or -1 for a body that is not in it."
  ^long [^PushGrid index ^long eid]
  (PushGrid/slot index eid))

(defn bodies
  "Returns empty bodies for add-body to fill."
  ^Bodies []
  (Bodies.))

(defn add-body
  "Adds the body of entry to bodies b and returns b. The body steps
  this tick when ticks? is true."
  ^Bodies [^Bodies b [eid e :as entry] ticks?]
  (let [p (:pos e) [h t] (pushable-box e)]
    (Bodies/add b entry (long eid) (double h) (double t)
                (v/x p) (v/y p) (v/z p)
                (came e) (rank eid e) (boolean ticks?))))

(defn joined-bodies
  "Adds the bodies of o to bodies b after its own and returns b. No
  body may be added to o after."
  ^Bodies [^Bodies b ^Bodies o]
  (Bodies/joined b o))

(defn groups
  "Returns the indices of the bodies of b in groups that one tick of
  movement cannot bring together. Each group ascends by id, and the
  groups go by their first id."
  [^Bodies b]
  (Bodies/islands b))

(defn entries-of
  [^Bodies b ^ints g]
  (Bodies/entries b g))

(defn grid-of
  "Returns the index of the bodies of b at indices g, which ascend by
  id. Its bodies meet regardless of team until teamed."
  ^PushGrid [^Bodies b ^ints g]
  (Bodies/grid b g))

(defn ticks-of
  ^booleans [^Bodies b ^ints g]
  (Bodies/ticking b g))

(def ^:private rule-ids
  {:always 0 :never 1 :push-other-teams 2 :push-own-team 3})

(defn teamed
  "Returns index in which bodies es, in its order, meet only as the
  collision rules of teams let them."
  ^PushGrid [^PushGrid index teams es]
  (if (every? #(= :always (:collision %)) teams)
    index
    (let [ids (zipmap (map :name teams) (range))
          ts (mapv (fn [[eid e]] (team/of-entity teams eid e)) es)
          rules (byte-array (map #(rule-ids (:collision %) 0) ts))
          in (int-array (map #(get ids (:name %) -1) ts))]
      (PushGrid/teamed index rules in))))

(defn index-of
  "Returns the index in which a body finds every body near enough to
  shove it, each body stepping this tick, as teams let them meet.
  Each move of a body changes it in place."
  (^PushGrid [entries] (index-of entries nil))
  (^PushGrid [entries teams]
   (let [es (if (by-id? entries) entries (sort-by first entries))
         b (reduce #(add-body %1 %2 true) (bodies) es)]
     (teamed (grid-of b (int-array (range (count es)))) teams es))))

(defn- scan [^PushGrid index eid p half height hi]
  (if index
    (PushGrid/shoves index (v/x p) (v/y p) (v/z p) (double half)
                     (double height) (long eid) (long hi))
    []))

(defn touching
  "Returns the ids of the bodies whose boxes overlap the box
  of body eid."
  [^PushGrid index eid e half height]
  (let [p (:pos e)]
    (PushGrid/touching index (v/x p) (v/y p) (v/z p) (double half)
                       (double height) (long eid))))

(defn shoves-at
  "Returns the shoves between body eid at pos and each body it
  overlaps, as [id dx dz] in the order the bodies meet. Body eid
  takes dx dz and body id the opposite. A body whose center lies
  within 0.01 of pos along both x and z gives no shove."
  [index eid pos half height]
  (scan index eid pos half height Long/MAX_VALUE))

(defn shoves
  "Returns the shoves of body eid where e stands, as shoves-at does."
  [index eid e half height]
  (shoves-at index eid (:pos e) half height))

(defn before
  "Returns the shoves between body eid where e stands and each body
  of lower id that stepped this tick, as shoves-at does."
  [index eid e half height]
  (scan index eid (:pos e) half height (long eid)))

(defn pinned
  "Returns index with each body kept in the column it stands in.
  Readers may share it while bodies move no further than reach."
  ^PushGrid [^PushGrid index reach]
  (PushGrid/pinned index (double reach)))

(defn turns
  "Calls mind before body with the slot of each body of the pinned
  index, in parallel, and join with each slot in order. The call of
  body with a slot waits for the calls with each lower slot whose
  body it could meet in a tick in which no body moves further than
  reach along x or z. The call of join with a slot follows the calls
  of body with it and each lower slot, and no two calls
  of join overlap."
  [^PushGrid index reach mind body join]
  (Turns/run index (double reach) mind body join))

(defn within?
  "Returns true when body e, now e2, kept its box and moved no
  further than reach along x or z."
  [e e2 reach]
  (let [p (:pos e) q (:pos e2) r (double reach)]
    (and (<= (Math/abs (- (v/x q) (v/x p))) r)
         (<= (Math/abs (- (v/z q) (v/z p))) r)
         (== (pushable-half e) (pushable-half e2))
         (== (pushable-height e) (pushable-height e2)))))

(def ^:private ^:const player-reach 2.0)

(defn- near? [a b]
  (and (< (Math/abs (- (v/x a) (v/x b))) player-reach)
       (< (Math/abs (- (v/z a) (v/z b))) player-reach)))

(defn- shoved-by? [chunks p [_ e]]
  (and (near? (:pos p) (:pos e)) (pushable? chunks e)
       (not (mobs/death-ends? e))))

(defn- taken [es [eid dx dz]]
  (let [vel (:vel (get es eid))
        v (v/v3 (- (v/x vel) (double dx)) (v/y vel)
                (- (v/z vel) (double dz)))]
    [:merge-entity eid {:vel v}]))

(def cramming-damage 6.0)

(def ^:private ^:const cramming-key 0x63726d)

(defn cramming-draw?
  "Returns true when body eid, crowded in tick t, draws the one in
  four that tests it for cramming."
  [t eid]
  (< (random/of-longs t eid cramming-key) 0.25))

(defn crowded?
  "Returns true when n pushable bodies in a box are a crowd under the
  max_entity_cramming rule of world."
  [world n]
  (let [m (long (get (:rules world) :max-entity-cramming 24))]
    (and (pos? m) (> (long n) (dec m)))))

(def ^:private crush {:type :cramming})

(defn- crammed [world peid p n]
  (when (and (alive? p) (cramming-draw? (:tick world) peid)
             (crowded? world n))
    (when-let [ds (hurt/damage-deltas
                    world peid p cramming-damage crush)]
      (let [h (hurt/hurt-now world peid p ds)]
        (into ds (hurt/report-deltas world peid h))))))

(defn- body-type? [t]
  (or (identical? :player t) (mobs/mob-type? t)))

(defn- crowd-of [world [peid p :as entry]]
  (let [es (:entities world)
        [h ht] (pushable-box p)
        o (:pos p) h (double h)
        lo (v/v3 (- (v/x o) h) (v/y o) (- (v/z o) h))
        hi (v/v3 (+ (v/x o) h) (+ (v/y o) (double ht)) (+ (v/z o) h))
        ok? (fn [[eid e :as x]]
              (and (not= eid peid) (body-type? (:type e))
                   (not (game-mode/spectator? e))
                   (shoved-by? (:chunks world) p x)))
        body (fn [eid]
               (when-let [e (get es eid)] (MapEntry/create eid e)))]
    (into [entry] (comp (keep body) (filter ok?))
          (sections/within (sections/index world) lo hi))))

(defn- shoved-mobs [es]
  (comp (filter #(mobs/mob-type? (:type (get es (nth % 0)))))
        (map #(taken es %))))

(defn player-shoves
  "Returns the deltas of the cramming of player entry p and of the
  shoves it gives the mobs its box overlaps as its tick ends. The
  player takes none of them back, and a spectator gives none."
  [world [peid p :as entry]]
  (when-not (game-mode/spectator? p)
    (let [bs (crowd-of world entry)]
      (when (next bs)
        (let [[h ht] (pushable-box p)
              index (index-of bs (:teams world))
              n (count (touching index peid p h ht))]
          (into (vec (crammed world peid p n))
                (shoved-mobs (:entities world))
                (shoves index peid p h ht)))))))
