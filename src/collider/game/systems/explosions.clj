(ns collider.game.systems.explosions
  "Explosion blast effects on blocks and entities."
  (:require [clojure.data.int-map :as i]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.block.tnt :as tnt]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.mobs :as mobs]
            [collider.game.out :as out]
            [collider.game.state :as state]
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

(defn- entity-density [seen center e p]
  (let [[half height] (entity-box e)]
    (explosion/exposed seen center p half height)))

(defn- density-now [seen craters e p spec]
  (let [[half height] (entity-box e)]
    (if (explosion/stale? seen craters p half height)
      (explosion/exposed-now seen craters p half height)
      spec)))

(defn- impulse-at [center e p d12 density power]
  (let [ey (+ (double (v/y p)) (entity/eye-height e))]
    (blast-impulse center (v/x p) ey (v/z p) d12 density power)))

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

(defn- dist-sq ^double [p ^double cx ^double cy ^double cz]
  (let [dx (- (double (v/x p)) cx)
        dy (- (double (v/y p)) cy)
        dz (- (double (v/z p)) cz)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- reach-of [{:keys [later after]} [cx cy cz] power]
  (let [cx (double cx) cy (double cy) cz (double cz)
        power (double power)
        reach (+ (* 2.0 power) 2.0)
        reach-sq (* reach reach)
        after (if after (long after) Long/MIN_VALUE)
        start (fn [oid]
                (when (> (long oid) after)
                  (when-some [p (get later oid)]
                    (when (< (dist-sq p cx cy cz) reach-sq) p))))]
    (fn [[oid o]]
      (let [p (or (start oid) (:pos o))
            d12 (/ (Math/sqrt (dist-sq p cx cy cz)) (* 2.0 power))]
        (when (<= d12 1.0) [oid o p d12])))))

(defn- kb-candidates [index [cx cy cz] power reach]
  (let [x (long (Math/floor (double cx)))
        y (long (Math/floor (double cy)))
        z (long (Math/floor (double cz)))]
    (sort-by first
             (into [] (comp (mapcat #(get index %)) (keep reach))
                   (cell-keys x y z (cell-reach power))))))

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

(defn- hurt-item [hurt oid o dmg]
  (cond-> hurt
          (and (= :item (:type o)) (hurtable? o))
          (assoc oid (state/hurt o (double dmg)))))

(defn- blast-one [[motions ds hurt] [oid o] kb dmg]
  (cond
    (= :player (:type o))
    [(cond-> motions (not (:flying o)) (assoc oid kb)) ds hurt]
    (item-dies? o dmg)
    [motions (conj ds [:remove-entity oid]) (assoc hurt oid :gone)]
    :else [motions (pushed-deltas ds oid o kb dmg)
           (hurt-item hurt oid o dmg)]))

(defn- seen-bodies [seen center cands]
  (mapv (fn [[_ o p :as c]] (conj c (entity-density seen center o p)))
        cands))

(defn- knock [seen craters center power o p d12 spec]
  (let [density (density-now seen craters o p spec)]
    (when (pos? (double density))
      (impulse-at center o p d12 density power))))

(defn- blast-step [seen craters center power]
  (fn [acc [oid o p d12 spec]]
    (let [o (get (nth acc 2) oid o)
          kb (when-not (= :gone o)
               (knock seen craters center power o p d12 spec))]
      (if-let [[v dmg] kb]
        (blast-one acc [oid o] v dmg)
        acc))))

(defn- blast-deltas [craters hurt {:keys [seen cands power]} center]
  (reduce (blast-step seen craters center power) [{} [] hurt] cands))

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

(defn- here-block ^long [[rg craters] [x y z]]
  (explosion/read-now rg craters (long x) (long y) (long z)))

(defn- below-block ^long [[rg craters] [x y z]]
  (explosion/read-now rg craters (long x) (dec (long y)) (long z)))

(defn- below-pos [[x y z]]
  [(long x) (dec (long y)) (long z)])

(defn- fire-here? [now gone p seed below]
  (and (< (double (random/of-key [seed p :fire])) (/ 1.0 3.0))
       (or (contains? gone p) (zero? (here-block now p)))
       (not (contains? gone (below-pos p)))
       (block/solid-render? below)))

(defn- fire-on ^long [^long below]
  (if (block/tagged? below "soul_fire_base_blocks")
    (block/state :soul-fire)
    (fire/fire-state 0)))

(defn- fire-cells [now affected gone seed]
  (into []
        (keep (fn [p]
                (let [below (below-block now p)]
                  (when (fire-here? now gone p seed below)
                    [p (fire-on below)]))))
        affected))

(defn- chain-cells [now affected gone primed tnt?]
  (when tnt?
    (into []
          (comp (filter (fn [p] (tnt/tnt-state? (here-block now p))))
                (remove primed)
                (remove gone))
          affected)))

(defn- explosion-reader [world [cx cy cz]]
  (let [pos [(long (double cx)) (long (double cy))
             (long (double cz))]
        read (fn [id] (chunks/read-absent world id))]
    (explosion/block-reader (:chunks world) pos read)))

(defn- break-cells [world now cells gone primed source]
  (if (interacts? world source)
    (let [tnt? (get-in world [:rules :tnt-explodes] true)]
      [(chain-cells now cells gone primed tnt?)
       (into [] (comp (remove primed) (remove gone)) cells)])
    [[] []]))

(defn- with-read [world rg]
  (let [absent (fn [[id _]] (not (contains? (:chunks world) id)))
        payloads (filter absent (explosion/loaded-payloads rg))]
    (update world :chunks into
            (map (fn [[id p]] [id (:chunk p)])) payloads)))

(defn- changed-deltas [world rg destroy fires]
  (edit/shaped-deltas (with-read world rg)
                      (into (mapv (fn [p] [p 0]) destroy) fires)))

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
        (into (changed-deltas world rg destroy fires))
        (conj (explosion-msg b))
        (into pushes)
        (into (chain-deltas chains seed))
        (into (drop-deltas world rg destroy seed source power)))))

(defn- body-of [world index req]
  (let [{:keys [center power by]} req
        power (double (or power tnt/power))
        seed [(:tick world) (or by center)]
        rg (explosion-reader world center)
        rays (explosion/rays rg center power seed)
        seen (explosion/exposure rg center)
        reach (reach-of req center power)
        cands (kb-candidates index center power reach)]
    {:rg rg :power power :seed seed :rays rays :seen seen
     :cands (seen-bodies seen center cands)}))

(defn- bodies [world index reqs]
  (deltas/pmapcat (fn [req] [(body-of world index req)]) reqs 1 1))

(defn- blast-of [world body req gone primed craters]
  (let [{:keys [center source fire?]} req
        {:keys [rg power seed affected motions pushes]} body
        now [rg craters]
        [chains destroy]
        (break-cells world now (:blocks affected) gone primed source)
        gone' (into gone destroy)
        fires (if fire?
                (fire-cells now @(:cells affected) gone' seed)
                [])]
    {:center center :power power :seed seed :source source
     :affected affected :destroy destroy :chains chains
     :fires fires :motions motions :pushes pushes :gone gone'}))

(defn- crater! [craters {:keys [destroy fires]}]
  (doseq [[x y z] destroy] (explosion/crater! craters x y z 0))
  (doseq [[[x y z] st] fires] (explosion/crater! craters x y z st)))

(defn- reached-body [craters hurt body req]
  (let [center (:center req)
        [motions pushes hurt] (blast-deltas craters hurt body center)
        affected (explosion/reached (:rays body) craters center)]
    [(assoc body :motions motions :pushes pushes :affected affected)
     hurt]))

(defn- request-blasts [world craters [gone primed hurt bs] [req body]]
  (let [rg (:rg body)
        [body hurt] (reached-body craters hurt body req)
        b (blast-of world body req gone primed craters)
        b (assoc b :rg rg :loaded (explosion/loaded-payloads rg))]
    (crater! craters b)
    [(:gone b) (into primed (:chains b)) hurt (conj bs b)]))

(defn- blast-deltas-of [world {:keys [rg loaded] :as b}]
  (blast-acc world rg (vec (chunks/read-absent-deltas loaded)) b))

(defn- requests [d]
  (into [] (comp (filter (fn [delta] (= :explode (nth delta 0))))
                 (map second))
        (:world d)))

(defn- blastable [world]
  (remove (comp game-mode/spectator? val) (:entities world)))

(def ^:private ^:const job-count 12)

(defn- job-size ^long [^long n]
  (max 1 (quot (+ n (dec job-count)) job-count)))

(defn- job [world bs]
  #(into [] (mapcat (partial blast-deltas-of world)) bs))

(defn blasts
  "Returns the jobs that give the deltas of every blast requested
  this tick. The deltas come in request order."
  [world d]
  (let [reqs (requests d)]
    (when (seq reqs)
      (let [index (kb-index (blastable world))
            init [#{} (tnt/primed-origins world) {} []]
            pairs (map vector reqs (bodies world index reqs))
            step (partial request-blasts world (explosion/craters))
            done (nth (reduce step init pairs) 3)]
        (mapv (partial job world)
              (partition-all (job-size (count done)) done))))))

(defn explosions
  "Returns the deltas for every blast requested this tick.
  They come in request order."
  [world d]
  (deltas/run-seq (blasts world d)))
