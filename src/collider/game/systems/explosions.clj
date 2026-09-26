(ns collider.game.systems.explosions
  "Explosion blast effects on blocks and entities."
  (:require [clojure.data.int-map :as i]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.data :as data]
            [collider.world.blocks.fire :as fire]
            [collider.world.block :as block]
            [collider.world.space.explosion :as explosion]))

(set! *warn-on-reflection* true)

(defn- entity-box [e]
  (case (:type e)
    :player [0.3 1.8]
    :tnt [0.49 0.98]
    :item [0.125 0.25]
    (let [{:keys [half height]} (mobs/types (:type e))]
      [(or half 0.45) (or height 1.3)])))

(defn- blast-damage ^double [^double k ^double power]
  (Math/floor (+ 1.0 (* (/ (+ (* k k) k) 2.0) 7.0 2.0 power))))

(defn- blast-impulse [center px ey pz d12 density power]
  (let [dx (- (double px) (double (center 0)))
        dy (- (double ey) (double (center 1)))
        dz (- (double pz) (double (center 2)))
        d13 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (when (pos? d13)
      (let [k (* (- 1.0 (double d12)) (double density))]
        [[(* (/ dx d13) k) (* (/ dy d13) k) (* (/ dz d13) k)]
         (blast-damage k (double power))]))))

(defn- blast-distance ^double [p [cx cy cz] ^double power]
  (let [dx (- (double (v/x p)) (double cx))
        dy (- (double (v/y p)) (double cy))
        dz (- (double (v/z p)) (double cz))]
    (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
       (* 2.0 power))))

(defn- entity-density [read center e p]
  (let [[half height] (entity-box e)]
    (explosion/block-density read center p half height)))

(defn- impulse-at [center e p d12 density power]
  (let [ey (+ (double (v/y p)) (entity/eye-height e))]
    (blast-impulse center (v/x p) ey (v/z p) d12 density power)))

(defn- knockback [read center e p power]
  (let [d12 (blast-distance p center (double power))]
    (when (<= d12 1.0)
      (let [density (entity-density read center e p)]
        (when (pos? density)
          (impulse-at center e p d12 density power))))))

(def ^:private ^:const kb-cell 8)

(defn- kb-cell-key ^long [^long x ^long y ^long z]
  (bit-or (bit-shift-left (+ (bit-shift-right x 3) 524288) 26)
          (bit-shift-left (+ (bit-shift-right z 3) 524288) 6)
          (bit-and (bit-shift-right y 3) 63)))

(defn- pos-cell-key ^long [p]
  (kb-cell-key (long (Math/floor (double (v/x p))))
               (long (Math/floor (double (v/y p))))
               (long (Math/floor (double (v/z p))))))

(defn- kb-index [entries]
  (persistent!
    (reduce (fn [m [_ e :as entry]]
              (let [k (pos-cell-key (:pos e))]
                (assoc! m k (conj (get m k []) entry))))
            (transient (i/int-map))
            entries)))

(defn- cell-keys [^long x ^long y ^long z ^long n]
  (let [ds (range (- n) (inc n))]
    (for [dx ds dy ds dz ds]
      (kb-cell-key (+ x (* (long dx) kb-cell))
                   (+ y (* (long dy) kb-cell))
                   (+ z (* (long dz) kb-cell))))))

(defn- cell-reach ^long [power]
  (long (Math/ceil (/ (* 2.0 (double power)) kb-cell))))

(defn- kb-candidates [index [cx cy cz] power]
  (let [x (long (Math/floor (double cx)))
        y (long (Math/floor (double cy)))
        z (long (Math/floor (double cz)))]
    (->> (cell-keys x y z (cell-reach power))
         (map (fn [k] (get index k)))
         (apply concat)
         (sort-by first))))

(defn- blast-proof? [e]
  (and (= :item (:type e))
       (= "is_explosion" (data/resists (:item (:stack e))))))

(defn- hurtable? [e]
  (and (some? (:health e)) (not (blast-proof? e))))

(defn- item-dies? [e dmg]
  (and (= :item (:type e)) (hurtable? e)
       (>= (double dmg) (double (:health e)))))

(defn- pushed-deltas [ds oid o kb dmg]
  (cond-> (conj ds [:push oid kb])
          (hurtable? o) (conj [:damage oid dmg])))

(defn- blast-one [[motions ds] [oid o] kb dmg]
  (cond
    (= :player (:type o))
    [(cond-> motions (not (:flying o)) (assoc oid kb)) ds]
    (item-dies? o dmg) [motions (conj ds [:remove-entity oid])]
    :else [motions (pushed-deltas ds oid o kb dmg)]))

(defn- blast-deltas
  "Returns the knockback of each player and the deltas for every
  other entity the blast moves."
  [read index center power later]
  (let [step (fn [acc [oid o :as entry]]
               (let [p (get later oid (:pos o))]
                 (if-let [[kb dmg] (knockback read center o p power)]
                   (blast-one acc entry kb dmg)
                   acc)))]
    (reduce step [{} []] (kb-candidates index center power))))

(defn- explosion-pitch ^double [seed]
  (let [r (- (random/of-key [seed :p1])
             (random/of-key [seed :p2]))]
    (* 0.7 (+ 1.0 (* 0.2 r)))))

(def ^:private decay-rules
  {:tnt :tnt-explosion-drop-decay
   :block :block-explosion-drop-decay
   :mob :mob-explosion-drop-decay})

(defn- decay-rule [source]
  (get decay-rules source :block-explosion-drop-decay))

(defn- decay-radius [world source power]
  (when (get-in world [:rules (decay-rule source)]
                (not= :tnt source))
    power))

(defn- interacts? [world source]
  (or (not= :mob source) (get-in world [:rules :mob-griefing] true)))

(defn- drop-deltas [world rg destroy seed source power]
  (when (get-in world [:rules :block-drops] true)
    (let [radius (decay-radius world source power)
          stacks (explosion/stacks rg destroy seed radius)]
      (map-indexed (fn [i [pos stack]]
                     [:spawn-entity (items/popped world pos stack i)])
                   stacks))))

(defn- here-block ^long [rg [x y z]]
  (explosion/read-block rg (long x) (long y) (long z)))

(defn- below-block ^long [rg [x y z]]
  (explosion/read-block rg (long x) (dec (long y)) (long z)))

(defn- below-pos [[x y z]]
  [(long x) (dec (long y)) (long z)])

(defn- fire-here? [rg gone p seed below]
  (and (< (double (random/of-key [seed p :fire])) (/ 1.0 3.0))
       (or (contains? gone p) (zero? (here-block rg p)))
       (not (contains? gone (below-pos p)))
       (block/solid-render? below)))

(defn- fire-on ^long [^long below]
  (if (block/tagged? below "soul_fire_base_blocks")
    (block/state :soul-fire)
    (fire/fire-state 0)))

(defn- fire-cells [rg affected gone seed]
  (into []
        (keep (fn [p]
                (let [below (below-block rg p)]
                  (when (fire-here? rg gone p seed below)
                    [p (fire-on below)]))))
        affected))

(defn- chain-cells [rg affected gone primed tnt?]
  (when tnt?
    (into []
          (comp (filter (fn [p] (tnt/tnt-state? (here-block rg p))))
                (remove primed)
                (remove gone))
          affected)))

(defn- explosion-reader [world [cx cy cz]]
  (let [pos [(long (double cx)) (long (double cy))
             (long (double cz))]
        read (fn [id] (chunks/read-absent world id))]
    (explosion/block-reader (:chunks world) pos read)))

(defn- break-cells [world rg cells gone primed source]
  (if (interacts? world source)
    (let [tnt? (get-in world [:rules :tnt-explodes] true)]
      [(chain-cells rg cells gone primed tnt?)
       (into [] (comp (remove primed) (remove gone)) cells)])
    [[] []]))

(defn- destroy-delta [destroy]
  [:set-blocks (mapv (fn [p] [p 0]) destroy)])

(defn- chain-delta [p seed]
  [:spawn-entity (assoc (tnt/chain-primed p seed) :origin nil)])

(defn- chain-deltas [chains seed]
  (map (fn [p] (chain-delta p seed)) chains))

(defn- explosion-msg [{:keys [center power affected motions seed]}]
  (let [n (:count affected)
        pitch (explosion-pitch seed)]
    (out/all (out/explosion center power n motions pitch))))

(defn- blast-acc [world rg acc b]
  (let [{:keys [destroy chains fires pushes seed source power]} b]
    (-> acc
        (cond-> (seq destroy) (conj (destroy-delta destroy)))
        (conj (explosion-msg b))
        (into pushes)
        (into (chain-deltas chains seed))
        (into (drop-deltas world rg destroy seed source power))
        (cond-> (seq fires) (conj [:set-blocks fires])))))

(defn- blast-of [world rg index req gone primed]
  (let [{:keys [center power source fire? by later]} req
        power (double (or power tnt/power))
        seed [(:tick world) (or by center)]
        affected (explosion/affected-blocks rg center power seed)
        [chains destroy]
        (break-cells world rg (:blocks affected) gone primed source)
        gone' (into gone destroy)
        [motions pushes] (blast-deltas rg index center power later)
        fires (if fire?
                (fire-cells rg @(:cells affected) gone' seed)
                [])]
    {:center center :power power :seed seed :source source
     :affected affected :destroy destroy :chains chains
     :fires fires :motions motions :pushes pushes :gone gone'}))

(defn- request-deltas [world index [gone primed acc] req]
  (let [rg (explosion-reader world (:center req))
        b (blast-of world rg index req gone primed)
        acc (into acc (chunks/read-absent-deltas
                        (explosion/loaded-payloads rg)))]
    [(:gone b) (into primed (:chains b))
     (blast-acc world rg acc b)]))

(defn- requests [d]
  (into [] (comp (filter (fn [delta] (= :explode (nth delta 0))))
                 (map second))
        (:world d)))

(defn- blastable
  "ServerExplosion.hurtEntities takes the entities
  Level.getEntities finds, which are no spectators."
  [world]
  (remove (comp game-mode/spectator? val) (:entities world)))

(defn explosions
  "Returns the deltas for every blast requested this tick.
  They come in request order."
  [world d]
  (let [reqs (requests d)]
    (when (seq reqs)
      (let [index (kb-index (blastable world))
            init [#{} (tnt/primed-origins world) []]]
        (nth (reduce (partial request-deltas world index) init reqs)
             2)))))
