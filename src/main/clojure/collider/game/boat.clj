(ns collider.game.boat
  "Players boarding, steering and leaving boats."
  (:require [clojure.string :as str]
            [collider.game.apply :as apply]
            [collider.game.boat.motion :as motion]
            [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.block.screen :as screen]
            [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.inventory :as inventory]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mode :as mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.game.reach :as reach]
            [collider.math :as math]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn- chest? [e] (str/includes? (name (:type e)) "chest"))

(defn- raft? [e]
  (contains? #{:bamboo-raft :bamboo-chest-raft} (:type e)))

(defn passengers [world eid e]
  (filterv #(let [p (get (:entities world) %)]
              (and p (= eid (:vehicle p))))
           (:passengers e)))

(defn- in-reach? [p e]
  (let [[half height] (size/box e)
        [ex ey ez] (reach/eye-pos p)
        [x y z] (:pos e)
        width (* 2.0 (double half))
        dx (reach/axis-gap ex (- x half) width)
        dy (reach/axis-gap ey y height)
        dz (reach/axis-gap ez (- z half) width)
        r (+ (player/entity-reach p) 3.0)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- seat-offset [e eid]
  (if (> (count (:passengers e)) 1)
    (math/f32 (if (= eid (first (:passengers e))) 0.2 -0.6))
    (if (chest? e) (math/f32 0.15) 0.0)))

(defn- seat [e eid rider]
  (let [height (size/height (:type e))
        y (if (raft? e) (math/fmul height (math/f32 0.8888889))
              (math/fdiv height 3.0))
        z (seat-offset e eid)
        a (math/fmul (- (double (:yaw e))) math/deg->rad)
        p (:pos e)
        drop (if (entity/player? rider) 0.6 0.0)]
    (v/v3 (+ (v/x p) (* z (math/sin a)))
          (- (+ (v/y p) y) drop)
          (+ (v/z p) (* z (math/cos a))))))

(defn- seated [world eid e]
  (for [pid (passengers world eid e)
        :let [p (get (:entities world) pid) pos (seat e pid p)]
        :when (not (v/same? (:pos p) pos))]
    [:merge-entity pid {:pos pos}]))

(defn- escape [e p]
  (let [[half _] (size/box p)
        width (* 2.0 (size/half (:type e)))
        width (math/fmul width (math/f32 (Math/sqrt 2.0)))
        distance (/ (+ width (* 2.0 half) (math/f32 1.0E-5)) 2.0)
        a (math/fmul (double (:yaw p)) math/deg->rad)
        x (- (math/sin a)) z (math/cos a)
        scale (max (Math/abs x) (Math/abs z))]
    [(+ (v/x (:pos e)) (/ (* x distance) scale))
     (+ (v/z (:pos e)) (/ (* z distance) scale))]))

(defn- block-top [chunks cell]
  (let [st (chunk/at chunks cell)]
    (reduce (fn [top [_ _ _ _ hi _]] (max top (/ hi 16.0)))
            Double/NEGATIVE_INFINITY (block/collision-boxes st))))

(defn- floor-height [chunks [x y z :as cell]]
  (let [top (block-top chunks cell)]
    (if (> top Double/NEGATIVE_INFINITY)
      top
      (let [below (block-top chunks [x (dec y) z])]
        (if (>= below 1.0) (dec below) Double/NEGATIVE_INFINITY)))))

(defn- floors [chunks x z cell]
  (for [c [cell (update cell 1 dec)]
        :let [h (double (floor-height chunks c))]
        :when (and (Double/isFinite h) (< h 1.0))]
    (v/v3 x (+ (double (nth c 1)) h) z)))

(defn- fits? [world p pose pos]
  (let [[half height] (size/type-box :player pose)
        x (v/x pos) y (v/y pos) z (v/z pos)
        box [(- x half) y (- z half)
             (+ x half) (+ y height) (+ z half)]
        border (double chunk/world-border)]
    (and (<= (- border) (- x half) (+ x half) border)
         (<= (- border) (- z half) (+ z half) border)
         (phys/box-free? (:chunks world) box (v/y (:pos p))
                        (phys/context p)))))

(defn- dismount-at [world e p]
  (let [[x z] (escape e p)
        top (+ (v/y (:pos e)) (size/height (:type e)))
        cell [(math/floor x) (math/floor top) (math/floor z)]
        chunks (:chunks world)
        water? (block/water? (chunk/at chunks (update cell 1 dec)))
        candidates (when-not water?
                     (floors chunks x z cell))]
    (or (first (for [pose [:standing :crouching :swimming]
                    pos candidates :when (fits? world p pose pos)]
                {:pos pos :pose pose}))
        {:pos (v/v3 (v/x (:pos e)) top (v/z (:pos e)))})))

(defn- passenger-effect [eid before after]
  (out/all (out/set-passengers eid after before)))

(defn passenger-deltas [world eid e]
  (let [ps (passengers world eid e)]
    (when (not= ps (vec (:passengers e)))
      [[:merge-entity eid {:passengers ps}]
       (passenger-effect eid (:passengers e) ps)])))

(defn dismount-deltas [world eid p]
  (when-let [bid (:vehicle p)]
    (let [e (get (:entities world) bid)
          ps (filterv #(not= eid %) (:passengers e))
          at (if e (dismount-at world e p) {:pos (:pos p)})
          m {:vehicle nil :vehicle-first-good nil
             :vehicle-last-good nil
             :boarding-until (+ (:tick world) 60)}]
      (concat
        [[:merge-entity eid (merge m at)]
         [:teleport eid (:pos at)]]
        (when e
          [[:merge-entity bid {:passengers ps}]
           (passenger-effect bid (:passengers e) ps)])
        [(out/to eid (out/teleport (:pos at) 0.0 0.0 504))]
        (signal/game-event :entity-dismount
                           (:pos (or e p)) eid)))))

(defn- may-board? [world p bid e]
  (and (entity/boat? (:type e)) (not= bid (:vehicle p))
       (entity/alive? p) (not (mode/spectator? p))
       (not (:sneaking? p)) (not (:vehicle e))
       (<= (long (:boarding-until p 0)) (long (:tick world)))
       (< (count (passengers world bid e)) (if (chest? e) 1 2))
       (< (double (:out-of-control e 0.0)) 60.0)
       (not (liquid/eye-in-water?
              (:chunks world) (:pos e) (size/height (:type e))))
       (in-reach? p e)))

(defn- board-deltas [world eid p bid e]
  (let [ps (conj (passengers world bid e) eid)
        e' (assoc e :passengers ps)
        m (cond-> {:vehicle bid :pos (seat e' eid p)
                   :vel (v/v3 0.0 0.0 0.0) :fall 0.0}
            (entity/player? p)
            (assoc :pose :standing
                   :vehicle-first-good nil
                   :vehicle-last-good nil))]
    (concat (dismount-deltas world eid p)
            [[:merge-entity bid {:passengers ps}]
             [:merge-entity eid m]
             (passenger-effect bid (:passengers e) ps)]
            (seated world bid e')
            (signal/game-event :entity-mount (:pos e) eid))))

(declare open-chest)

(defn interact-deltas [world [_ eid bid _ sneaking?]]
  (when-let [p (get (:entities world) eid)]
    (let [e (get (:entities world) bid)
          p (assoc p :sneaking? (boolean sneaking?))]
      (concat
        (when (or (:vehicle p) (entity/boat? (:type e)))
          [[:merge-entity eid {:sneaking? (:sneaking? p)}]])
        (cond
          (and (:vehicle p) (:sneaking? p))
          (dismount-deltas world eid p)
          (and e (may-board? world p bid e))
          (board-deltas world eid p bid e)
          (and e (chest? e) (in-reach? p e))
          (open-chest world eid e bid))))))

(defn input-deltas [world [_ eid]]
  (let [p (get (:entities world) eid)]
    (when (and (:vehicle p) (:sneaking? p))
      (dismount-deltas world eid p))))

(defn move-deltas [world [_ eid :as ev]]
  (let [ds (vec (motion/move-deltas world ev))
        bid (get-in world [:entities eid :vehicle])]
    (if (some #(and (= :merge-entity (first %))
                    (= bid (second %))) ds)
      (let [w (first (apply/applied world ds))]
        (into ds (seated w bid (get (:entities w) bid))))
      ds)))

(defn- eye-inside? [world eid e]
  (let [eye (reach/eye-pos e)]
    (some (fn [[id o]]
            (when (and (not= id eid) (:pos o))
              (let [[half height] (or (size/box o) [0.0 0.0])
                    x (v/x (:pos o)) y (v/y (:pos o))
                    z (v/z (:pos o))
                    [ex ey ez] eye]
                (and (<= (- x half) ex (+ x half))
                     (<= y ey (+ y height))
                     (<= (- z half) ez (+ z half))))))
          (:entities world))))

(defn- room? [world born]
  (let [[half height] (size/box born)]
    (phys/free? (:chunks world) (:pos born) half height 0.0 0.0 0.0)))

(defn place-deltas
  "Returns the deltas of player eid setting the boat in its hand
  where it looks. Nothing, when the spot is blocked or the look
  misses."
  [world eid e]
  (when-let [hit (reach/clip world e true)]
    (when-not (eye-inside? world eid e)
      (let [kind (:item (player/hand-stack e (:use-hand e)))
            born {:type kind :pos (:at hit) :vel [0.0 0.0 0.0]
                  :yaw (double (:yaw e 0.0)) :pitch 0.0
                  :on-ground false}]
        (when (and (entity/boat? kind) (room? world born))
          (concat [[:spawn-entity born]]
                  (inventory/spent-deltas eid e (:use-hand e))
                  (signal/game-event :entity-place (:at hit) eid)))))))

(defn- dropped [world e]
  (let [pos (:pos e)
        stacks (cons {:item (:type e) :count 1}
                     (remove nil? (:cargo e)))]
    (map-indexed
      (fn [i s]
        [:spawn-entity
         (entity/item pos (entity/pop-velocity [(:tick world) i]) s)])
      stacks)))

(defn- broken [world tid e drops?]
  (concat
    (mapcat (fn [pid]
              (dismount-deltas world pid
                               (get (:entities world) pid)))
            (passengers world tid e))
    [[:remove-entity tid]]
    (when (and drops? (get-in world [:rules :entity-drops] true))
      (dropped world e))))

(defn- struck [world tid e a]
  (let [d (attribute/value a (:effects a) :attack-damage)
        d (if (pos? d) d 1.0)
        hull (+ (double (:hull e 0.0)) (* 10.0 d))]
    (if (> hull 40.0)
      (broken world tid e true)
      [[:merge-entity tid
        {:hull hull :hurt-time 10
         :hurt-dir (- (long (:hurt-dir e 1)))}]])))

(defn attack-deltas
  "Returns the deltas of player eid hitting boat tid."
  [world [_ eid tid]]
  (let [a (get (:entities world) eid)
        e (get (:entities world) tid)]
    (when (and a e (entity/boat? (:type e))
               (not (mode/spectator? a)) (in-reach? a e))
      (if (mode/instabuild? a)
        (broken world tid e false)
        (struck world tid e a)))))

(def ^:private ^:table kept-out
  (delay (set (map data/kebab
                   (data/tag-values "entity_type"
                                    "cannot_be_pushed_onto_boats")))))

(defn- seats [e] (if (chest? e) 1 2))

(defn- player-drives? [world e]
  (entity/player? (get (:entities world) (first (:passengers e)))))

(defn- wide? [boat other]
  (let [[bh] (size/box boat)
        [oh] (size/box other)]
    (or (nil? oh) (not (< (* 2.0 (double oh)) (* 2.0 (double bh)))))))

(defn- touches? [boat other]
  (let [[bh bht] (size/box boat)
        [oh oht] (size/box other)
        bx (double (v/x (:pos boat)))
        by (double (v/y (:pos boat)))
        bz (double (v/z (:pos boat)))
        ox (double (v/x (:pos other)))
        oy (double (v/y (:pos other)))
        oz (double (v/z (:pos other)))]
    (and (< (- bx bh 0.2) (+ ox oh))
         (> (+ bx bh 0.2) (- ox oh))
         (< (+ by 0.01) (+ oy oht))
         (> (- (+ by bht) 0.01) oy)
         (< (- bz bh 0.2) (+ oz oh))
         (> (+ bz bh 0.2) (- oz oh)))))

(defn- take-on [world eid e other-id other]
  (let [ps (conj (vec (:passengers e)) other-id)
        e' (assoc e :passengers ps)]
    [e' (concat [[:merge-entity eid {:passengers ps}]
                 [:merge-entity other-id
                  {:vehicle eid :pos (seat e' other-id other)
                   :vel (v/v3 0.0 0.0 0.0) :fall 0.0}]
                 (passenger-effect eid (:passengers e) ps)]
                (signal/game-event :entity-mount (:pos e) other-id))]))

(defn- wanted? [world e other-id other]
  (and (mobs/mob-type? (:type other))
       (not (contains? @kept-out (:type other)))
       (not (:vehicle other))
       (entity/alive? other)
       (not (wide? e other))
       (touches? e other)
       (not (some #{other-id} (:passengers e)))))

(defn gather
  "Returns the deltas of the mobs boat eid, e, takes on this tick.
  A boat a player steers, or one whose eyes are under water, takes
  none."
  [world eid e]
  (when-not (or (player-drives? world e)
                (liquid/eye-in-water? (:chunks world) (:pos e)
                                      (size/height (:type e))))
    (loop [e e ids (sort (keys (:entities world))) acc []]
      (if (or (>= (count (:passengers e)) (seats e)) (empty? ids))
        acc
        (let [id (first ids) other (get (:entities world) id)]
          (if (and other (wanted? world e id other))
            (let [[e' ds] (take-on world eid e id other)]
              (recur e' (rest ids) (into acc ds)))
            (recur e (rest ids) acc)))))))

(defn- chest-menu [e bid]
  {:kind :boat :eid bid :type :generic-9x3 :rows 3 :slots 27
   :pos (mapv #(Math/floor (double %)) (:pos e))
   :title {:translate
           (str "entity.minecraft."
                (str/replace (name (:type e)) "-" "_"))}})

(defn- open-chest [world eid e bid]
  (screen/entity-open-deltas world eid (chest-menu e bid)))

(defn player-deltas [world [eid p]]
  (when-let [bid (:vehicle p)]
    (let [e (get (:entities world) bid)]
      (if (or (nil? e) (:sneaking? p) (not (entity/alive? p)))
        (dismount-deltas world eid p)
        [[:merge-entity eid
          (cond-> {:pos (seat e eid p) :vel (v/v3 0.0 0.0 0.0)
                   :pose :standing :fall 0.0}
            (= eid (first (:passengers e)))
            (assoc :vehicle-first-good (:pos e)
                   :vehicle-last-good (:pos e)))]]))))
