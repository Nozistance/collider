(ns collider.game.systems.tnt

  (:require [collider.rnd :as rnd]
            [collider.vec :as v]
            [clojure.data.int-map :as i]
            [collider.game.entity :as entity]
            [collider.game.mobs :as mobs]
            [collider.game.state :as state]
            [collider.game.tnt :as tnt]
            [collider.proto.packets.play :as play]
            [collider.world.explosion :as explosion]
            [collider.world.gen :as gen]
            [collider.world.liquid :as liquid]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private ^:const tnt-half 0.49)
(def ^:private ^:const tnt-height 0.98)
(defn- liquid-push [world pos]
  (liquid/entity-push (:chunks world) gen/flat-chunk pos tnt-half tnt-height))

(defn- unblock-deltas [world eid e]
  (concat
   [[:set-block (:origin e) 0]]
   (state/broadcast world (play/block-change (:origin e) 0))
   [[:merge-entity eid {:origin nil :fuse (dec (long (:fuse e)))}]]))

(defn- step-deltas [world eid e]
  (let [kb (:kb e)
        [vx vy vz] (v/+ (:vel e) (or kb [0.0 0.0 0.0]))
        ^collider.world.phys.Move mv
        (phys/move (:chunks world) gen/flat-chunk (:pos e)
                   [(double vx) (- (double vy) 0.04) (double vz)]
                   tnt-half tnt-height)
        pos (.pos mv) vel (.vel mv) on-ground (.on-ground mv)
        [mx my mz] vel
        gf (if on-ground 0.7 1.0)]
    (cond-> [[:merge-entity eid
              {:pos pos
               :vel (v/+ [(* (double mx) 0.98 gf) (* (double my) 0.98)
                         (* (double mz) 0.98 gf)]
                        (liquid-push world pos))
               :on-ground on-ground
               :fuse (dec (long (:fuse e)))}]]
      kb (conj [:push eid (mapv - kb)]))))

(defn- entity-box [e]
  (case (:type e)
    :player [0.3 1.8]
    :tnt    [tnt-half tnt-height]
    :item   [0.125 0.25]
    (let [{:keys [half height]} (mobs/types (:type e))]
      [(or half 0.45) (or height 1.3)])))

(defn- knockback [read [cx cy cz :as center] e]
  (let [p (:pos e) px (v/x p) py (v/y p) pz (v/z p)
        dx (- (double px) (double cx))
        dy (- (double py) (double cy))
        dz (- (double pz) (double cz))
        d12 (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) (* 2.0 tnt/power))]
    (when (<= d12 1.0)
      (let [[half height] (entity-box e)
            density (explosion/block-density read center (:pos e) half height)]
        (when (pos? density)
          (let [ey (+ (double py) (entity/eye-height e))
                dy (- ey (double cy))
                d13 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
            (when (pos? d13)
              (let [k (* (- 1.0 d12) density)
                    dmg (Math/floor (+ 1.0 (* (/ (+ (* k k) k) 2.0) 8.0 2.0 tnt/power)))]
                [[(* (/ dx d13) k) (* (/ dy d13) k) (* (/ dz d13) k)] dmg]))))))))

(def ^:private ^:const kb-cell 8)
(defn- kb-cell-key ^long [^long x ^long y ^long z]

  (bit-or (bit-shift-left (+ (bit-shift-right x 3) 524288) 26)
          (bit-shift-left (+ (bit-shift-right z 3) 524288) 6)
          (bit-and (bit-shift-right y 3) 63)))

(defn- kb-index [entries]
  (persistent!
   (reduce (fn [m [_ e :as entry]]
             (let [p (:pos e) x (v/x p) y (v/y p) z (v/z p)
                   k (kb-cell-key (long (Math/floor (double x))) (long (Math/floor (double y))) (long (Math/floor (double z))))]
               (assoc! m k (conj (get m k []) entry))))
           (transient (i/int-map))
           entries)))

(defn- kb-candidates [index [cx cy cz]]
  (let [x (long (Math/floor (double cx))) y (long (Math/floor (double cy))) z (long (Math/floor (double cz)))]
    (->> (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]]
           (get index (kb-cell-key (+ x (* dx kb-cell)) (+ y (* dy kb-cell)) (+ z (* dz kb-cell)))))
         (apply concat)
         (sort-by first))))

(defn- knockback-deltas [read index center]
  (mapcat (fn [[oid o]]
            (when-let [[kb dmg] (knockback read center o)]
              (cond->
                      [[:push oid kb]]
                (and (some? (:health o)) (not= :player (:type o)))
                (conj [:damage oid dmg]))))
          (kb-candidates index center)))

(defn- dist2 ^double [[cx cy cz] p]
  (let [dx (- (v/x p) (double cx))
        dy (- (v/y p) (double cy))
        dz (- (v/z p) (double cz))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- near? [center p] (< (dist2 center p) 4096.0))
(def ^:private ^:const records-cap 32)

(defn- explosion-packets [read players center seed affected blasts]
  (let [pitch (* 0.7 (+ 1.0 (* 0.2 (- (rnd/rnd [seed :p1]) (rnd/rnd [seed :p2])))))
        sound (play/entity-sound "random.explode" center 4.0 (long (* 63.0 pitch)))
        recs  (if (<= (long blasts) records-cap) (vec affected) [])]
    (for [[eid e] players
          :when (near? center (:pos e))
          pkt [(play/explosion center tnt/power recs
                               (or (first (knockback read center e)) [0.0 0.0 0.0]))
               sound]]
      [:send eid pkt])))

(defn- explode-deltas [world others players pending blasts eid e]
  (let [[vx vy vz] (v/+ (:vel e) (or (:kb e) [0.0 0.0 0.0]))
        [x y z] (.pos ^collider.world.phys.Move
                      (phys/move (:chunks world) gen/flat-chunk (:pos e)
                                 [(double vx) (- (double vy) 0.04) (double vz)]
                                 tnt-half tnt-height))
        center   [(double x) (+ (double y) (/ tnt-height 16.0)) (double z)]
        seed     [(:tick world) eid]
        rg       (explosion/block-reader (:chunks world) gen/flat-chunk
                                         [(long x) (long y) (long z)])
        affected (explosion/affected-blocks rg center tnt/power seed)
        tnt-cell? (fn [[bx by bz]] (tnt/tnt-state? (explosion/read-block rg bx by bz)))
        chains   (into [] (comp (filter tnt-cell?) (remove pending)) affected)
        destroy  (into [] (remove pending) affected)]
    (concat
     [[:remove-entity eid]]
     (when (seq destroy) [[:set-blocks (mapv (fn [p] [p 0]) destroy)]])
     (explosion-packets rg players center seed affected blasts)
     (knockback-deltas rg others center)
     (map (fn [p] [:spawn-entity (assoc (tnt/chain-primed p seed) :origin nil :dedup [:tnt p])])
          chains))))

(defn tnt-system [world _events]
  (let [tnts   (into []
                     (filter (fn [[_ e]] (= :tnt (:type e))))
                     (sort-by key (:entities world)))
        fresh  (filterv (fn [[_ e]] (:origin e)) tnts)
        armed  (into [] (remove (fn [[_ e]] (:origin e))) tnts)
        due    (filterv (fn [[_ e]] (<= (long (:fuse e)) 0)) armed)
        moving (filterv (fn [[_ e]] (pos? (long (:fuse e)))) armed)
        others  (when (seq due)
                  (kb-index (filter (fn [[_ e]] (not= :player (:type e))) (:entities world))))
        players (when (seq due)
                  (vec (keep (fn [eid] (when-let [e (get-in world [:entities eid])]
                                         [eid e]))
                             (sort (vals (:players world))))))
        pending (when (seq due) (tnt/primed-origins world))]
    (-> []
        (into (map (fn [[eid e]] #(unblock-deltas world eid e))) fresh)
        (into (map (fn [[eid e]] #(explode-deltas world others players pending (count due) eid e))) due)
        (into (map (fn [[eid e]] #(step-deltas world eid e))) moving))))
