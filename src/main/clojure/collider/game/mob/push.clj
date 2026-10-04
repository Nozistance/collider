(ns collider.game.mob.push
  "Shoves between overlapping bodies."
  (:require [collider.game.entity.hurt :as hurt]
            [collider.game.entity.size :as size]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.blocks.climb :as climb]
            [collider.world.chunk :as chunk])
  (:import (collider.game.mob Bodies PushGrid Slots Turns)))

(set! *warn-on-reflection* true)

(def ^:private no-box [0.0 1.0])

(defn- pushable-box [e]
  (or (size/box e) no-box))

(defn- pushable-half ^double [e]
  (double (nth (pushable-box e) 0)))

(defn- pushable-height ^double [e]
  (double (nth (pushable-box e) 1)))

(defn alive?
  "Returns true when a body takes shoves.
  A dead body takes none but still steps and shoves the living."
  [e]
  (let [h (:health e)] (or (nil? h) (pos? (double h)))))

(defn body?
  "Returns true when the entity of entry shoves and takes shoves in
  a chunk of held."
  [held [_ e]]
  (and (or (= :player (:type e))
           (and (mobs/mob-type? (:type e))
                (not (mobs/death-ends? e))))
       (not (game-mode/spectator? e))
       (contains? held (chunk/pos-chunk (:pos e)))))

(defn- climbing? [chunks e]
  (and (not (:flying e)) (climb/on-climbable? chunks (:pos e))))

(defn pushable?
  "Returns true when body e takes shoves among chunks. A body that
  climbs takes none but still shoves the others."
  [chunks e]
  (and (alive? e) (not (climbing? chunks e))))

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
  "Returns index after body eid moved to its place in entry e."
  [^PushGrid index eid _old-pos e]
  (let [p (:pos e) [h t] (pushable-box e)]
    (PushGrid/moved index (long eid) (double h) (double t)
                    (double (v/x p))
                    (double (v/y p)) (double (v/z p)) (came e)
                    (rank eid e))))

(defn- same-section? [c c']
  (== (bit-shift-right (long (Math/floor (double c))) 4)
      (bit-shift-right (long (Math/floor (double c'))) 4)))

(defn arrived
  "Returns when body e, which moves to to in tick t, came into its
  entity section. The order of pushable entities follows it. A body
  that stays in its section keeps its old arrival, and one that moves
  arrives this tick after the bodies of lower id."
  [e to t eid]
  (let [from (:pos e)]
    (if (and (same-section? (v/x from) (v/x to))
             (same-section? (v/y from) (v/y to))
             (same-section? (v/z from) (v/z to)))
      (:arrived e)
      [(* 2 (long t)) (inc (* 2 (long eid)))])))

(defn slots
  "Returns the place of each body of entries by its id."
  ^Slots [entries]
  (let [n (count entries) eids (long-array n)]
    (dotimes [i n] (aset eids i (long (nth (nth entries i) 0))))
    (Slots/of eids)))

(defn slot
  "Returns the index of body eid in slots, or -1 when it has none."
  {:inline (fn [s eid]
             `(Slots/slot ~(with-meta s {:tag `Slots}) (long ~eid)))}
  ^long [^Slots s eid]
  (Slots/slot s (long eid)))

(defn bodies
  "Returns no bodies, to which add-body adds them in id order."
  ^Bodies []
  (Bodies.))

(defn add-body
  "Adds the body of entry to bodies b and returns b. The body steps
  this tick when ticks? is true."
  ^Bodies [^Bodies b [eid e :as entry] ticks?]
  (let [p (:pos e) [h t] (pushable-box e)]
    (Bodies/add b entry (long eid) (double h) (double t)
                (double (v/x p)) (double (v/y p)) (double (v/z p))
                (came e) (rank eid e) (boolean ticks?))))

(defn joined-bodies
  "Adds the bodies of o to bodies b after its own and returns b."
  ^Bodies [^Bodies b ^Bodies o]
  (Bodies/joined b o))

(defn groups
  "Returns the bodies of b in groups that one tick of movement cannot
  bring together, each as the indices of its bodies in b by id."
  [^Bodies b]
  (Bodies/islands b))

(defn entries-of
  [^Bodies b ^ints g]
  (Bodies/entries b g))

(defn grid-of
  "Returns the index of the bodies of b at indices g, as index-of
  gives it for their entries."
  ^PushGrid [^Bodies b ^ints g]
  (Bodies/grid b g))

(defn slots-of
  ^Slots [^Bodies b ^ints g]
  (Bodies/slots b g))

(defn ticks-of
  "Returns whether each body of b at indices g ticks this tick."
  ^booleans [^Bodies b ^ints g]
  (Bodies/ticking b g))

(defn index-of
  "Returns the index in which a body finds every body near enough to
  shove it, each body stepping this tick. Each move of a body changes
  it in place."
  ^PushGrid [entries]
  (let [es (if (by-id? entries) entries (sort-by first entries))
        b (reduce #(add-body %1 %2 true) (bodies) es)]
    (grid-of b (int-array (range (count es))))))

(defn islands
  "Returns the pushable bodies in groups that one tick of movement
  cannot bring together. Each group steps on its own."
  [world held]
  (let [f (fn [b entry]
            (if (body? held entry) (add-body b entry false) b))
        b (reduce f (bodies) (:entities world))]
    (mapv #(entries-of b %) (groups b))))

(defn- scan [^PushGrid index eid p half height hi]
  (if index
    (PushGrid/shoves index (double (v/x p)) (double (v/y p))
                     (double (v/z p)) (double half) (double height)
                     (long eid) (long hi))
    []))

(defn touching
  "Returns the ids of the bodies whose boxes overlap the box of body
  eid."
  [^PushGrid index eid e half height]
  (let [p (:pos e)]
    (PushGrid/touching index (double (v/x p)) (double (v/y p))
                       (double (v/z p)) (double half) (double height)
                       (long eid))))

(defn shoves
  "Returns the shoves of one run of the body over every body it meets.
  The run is the last part of its tick."
  [index eid e half height]
  (scan index eid (:pos e) half height Long/MAX_VALUE))

(defn shoves-at
  "Returns the shoves of body eid run to pos, as shoves does."
  [index eid pos half height]
  (scan index eid pos half height Long/MAX_VALUE))

(defn before
  "Returns the shoves that the bodies which stepped earlier this tick
  gave to this body. This body takes the opposite of what each of
  them took."
  [index eid e half height]
  (scan index eid (:pos e) half height (long eid)))

(defn pinned
  "Returns index with each body kept in the column it stands in.
  Readers may share it while bodies move no further than reach."
  ^PushGrid [^PushGrid index reach]
  (PushGrid/pinned index (double reach)))

(defn turns
  "Calls mind before body with the index of each body of the
  pinned index, in parallel, and join with each index in order. The
  call of body with a body waits for the calls with each body of
  lower id that it could meet in a tick in which no body moves
  further than reach along x or z. The call of join with a body
  follows the calls of body with it and each body of lower id."
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

(def ^:private mob-types (vec (keys mobs/types)))

(defn- near? [a b]
  (and (< (Math/abs (- (v/x a) (v/x b))) player-reach)
       (< (Math/abs (- (v/z a) (v/z b))) player-reach)))

(defn- shoved-by? [chunks p [_ e]]
  (and (pushable? chunks e) (not (mobs/death-ends? e))
       (near? (:pos p) (:pos e))))

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

(defn- crowd-of [world [peid p :as entry]]
  (let [ok? (fn [[eid e :as x]]
              (and (not= eid peid) (not (game-mode/spectator? e))
                   (shoved-by? (:chunks world) p x)))]
    (-> [entry]
        (into (filter ok?) (level/of-types world mob-types))
        (into (filter ok?) (level/player-entries world)))))

(defn player-shoves
  "Returns the deltas of the cramming of player entry p and of the
  shoves it gives the mobs its box overlaps as its tick ends. The
  player takes none of them back, and a spectator gives none."
  [world [peid p :as entry]]
  (when-not (game-mode/spectator? p)
    (let [bs (crowd-of world entry)]
      (when (next bs)
        (let [[h ht] (pushable-box p)
              index (index-of bs)
              n (count (touching index peid p h ht))
              es (:entities world)
              mob? #(mobs/mob-type? (:type (get es (nth % 0))))]
          (into (vec (crammed world peid p n))
                (comp (filter mob?) (map #(taken es %)))
                (shoves index peid p h ht)))))))
