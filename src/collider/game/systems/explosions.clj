(ns collider.game.systems.explosions
  "Explosion blast effects on blocks and entities."
  (:require [clojure.data.int-map :as i]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.block.tnt :as tnt]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.game-mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.hanging :as hanging]
            [collider.game.state :as state]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.hanging :as hanging-system]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.data :as data]
            [collider.world.blocks.fire :as fire]
            [collider.world.block :as block]
            [collider.world.phys :as phys]
            [collider.world.space.explosion :as explosion]))

(set! *warn-on-reflection* true)

(defn- blast-damage ^double [^double k ^double power]
  (let [twice (double (float (* 2.0 power)))]
    (double (float (+ 1.0 (* (/ (+ (* k k) k) 2.0) 7.0 twice))))))

(def ^:private ^:const unit-least (double (float 1.0E-5)))

(defn- blast-impulse [center px ey pz d12 density power]
  (let [dx (- (double px) (double (center 0)))
        dy (- (double ey) (double (center 1)))
        dz (- (double pz) (double (center 2)))
        d13 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
        k (* (- 1.0 (double d12)) (double density))
        dmg (blast-damage k (double power))]
    (if (< d13 unit-least)
      [[0.0 0.0 0.0] dmg]
      [[(* (/ dx d13) k) (* (/ dy d13) k) (* (/ dz d13) k)] dmg])))

(defn- density-now [seen craters e p look]
  (let [[half height] (entity/box e)
        flags (phys/context e)]
    (explosion/exposed-now seen craters look p half height flags)))

(defn- impulse-at [center e p d12 density power]
  (let [ey (cond-> (double (v/y p))
             (not= :tnt (:type e)) (+ (entity/eye-height e)))]
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
       (not (pos? (double (:health (state/hurt e (double dmg))))))))

(defn- hurt-item [hurt oid o dmg]
  (cond-> hurt
          (and (= :item (:type o)) (hurtable? o))
          (assoc oid (state/hurt o (double dmg)))))

(defn- moving? [[x y z]]
  (not (every? (fn [c] (zero? (double c))) [x y z])))

(defn- pushed-deltas [ds oid o kb dmg]
  (cond-> ds
          (moving? kb) (conj [:push oid kb])
          (hurtable? o) (conj [:damage oid dmg])))

(defn- blast-one [[motions ds hurt] [oid o] kb dmg]
  (cond
    (= :player (:type o))
    [(cond-> motions (not (:flying o)) (assoc oid kb)) ds hurt]
    (item-dies? o dmg)
    [motions (conj ds [:remove-entity oid]) (assoc hurt oid :gone)]
    :else [motions (pushed-deltas ds oid o kb dmg)
           (hurt-item hurt oid o dmg)]))

(defn- seen-body [seen [_ o p :as c]]
  (let [[half height] (entity/box o)
        flags (phys/context o)]
    (conj c (explosion/look seen p half height flags)
          half height flags)))

(defn- seen-bodies [seen cands]
  (mapv (partial seen-body seen) cands))

(defn- knock [seen craters center power o p d12 look]
  (let [density (density-now seen craters o p look)]
    (impulse-at center o p d12 density power)))

(defn- item-step [center power hurt oid o p d12 density]
  (let [[v dmg] (impulse-at center o p d12 density power)
        [_ ds hurt] (blast-one [{} [] hurt] [oid o] v dmg)]
    [hurt ds]))

(defn- hung? [o]
  (and (not= :gone o) (contains? hanging/types (:type o))))

(defn- body-now [broken seen craters center power hurt cand]
  (let [[oid o p d12 look half height flags] cand
        o (get hurt oid o)
        density (when-not (or (= :gone o) (hung? o))
                  (explosion/exposed-now seen craters look p half
                    height flags))]
    (cond
      (hung? o) [(assoc hurt oid :gone) (vec (broken oid o))]
      (nil? density) [hurt nil]
      (= :item (:type o))
      (item-step center power hurt oid o p d12 density)
      :else [hurt density])))

(defn- blast-step [broken seen craters center power]
  (fn [[hurt outs] cand]
    (let [[hurt out]
          (body-now broken seen craters center power hurt cand)]
      [hurt (conj outs out)])))

(defn- body-deltas [center power [motions ds] [[oid o p d12] out]]
  (cond
    (nil? out) [motions ds]
    (vector? out) [motions (into ds out)]
    :else (let [[v dmg] (impulse-at center o p d12 out power)]
            (pop (blast-one [motions ds nil] [oid o] v dmg)))))

(defn- knocks [{:keys [center power cands outs]}]
  (reduce (partial body-deltas center power) [{} []]
          (map vector cands outs)))

(def ^:private add-kb (fnil v/+ [0.0 0.0 0.0]))

(defn- fresh-hit [o kb dmg]
  (if (item-dies? o dmg)
    :gone
    (cond-> o
      (moving? kb) (update (if (= :tnt (:type o)) :kb :vel) add-kb kb)
      (hurtable? o) (state/hurt (double dmg)))))

(defn- fresh-step [seen craters center power reach]
  (fn [fresh i]
    (let [o (nth fresh i)]
      (if-let [[_ _ p d12] (when-not (= :gone o) (reach [i o]))]
        (let [[kb dmg] (knock seen craters center power o p d12 nil)]
          (assoc fresh i (fresh-hit o kb dmg)))
        fresh))))

(defn- blast-deltas
  [broken craters hurt {:keys [seen cands power]} center]
  (reduce (blast-step broken seen craters center power) [hurt []]
          cands))

(defn- fresh-blast [craters fresh {:keys [seen power]} center]
  (let [reach (reach-of {} center power)]
    (reduce (fresh-step seen craters center power reach) fresh
            (range (count fresh)))))

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

(defn- drop-specs [world rg destroy seed source power]
  (when (get-in world [:rules :block-drops] true)
    (let [radius (decay-radius world source power)
          stacks (explosion/stacks rg destroy seed radius)]
      (map-indexed (fn [i [p stack]] (items/popped world p stack i))
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

(defn- chain-spec [p seed]
  (assoc (tnt/chain-primed p seed) :origin nil))

(defn- spawn-specs [world rg b]
  (let [{:keys [destroy chains seed source power]} b]
    (-> []
        (into (map (fn [p] (chain-spec p seed))) chains)
        (into (drop-specs world rg destroy seed source power)))))

(defn- spawn-deltas [fresh [from to]]
  (into [] (comp (remove #{:gone}) (map (fn [e] [:spawn-entity e])))
        (subvec fresh from to)))

(defn- explosion-msg [{:keys [center power affected seed]} motions]
  (let [n (:count affected)
        pitch (explosion-pitch seed)]
    (out/all (out/explosion center power n motions pitch))))

(defn- blast-acc [world rg fresh acc b]
  (let [{:keys [destroy fires spawned]} b
        [motions pushes] (knocks b)]
    (-> acc
        (into (changed-deltas world rg destroy fires))
        (conj (explosion-msg b motions))
        (into pushes)
        (into (spawn-deltas fresh spawned)))))

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
     :cands (seen-bodies seen cands)}))

(defn- bodies [world index reqs]
  (deltas/pmapcat (fn [req] [(body-of world index req)]) reqs 1 1))

(defn- blast-of [world body req gone primed craters]
  (let [{:keys [center source fire?]} req
        {:keys [rg power seed affected cands outs]} body
        now [rg craters]
        [chains destroy]
        (break-cells world now (:blocks affected) gone primed source)
        gone' (into gone destroy)
        fires (if fire?
                (fire-cells now @(:cells affected) gone' seed)
                [])]
    {:center center :power power :seed seed :source source
     :affected affected :destroy destroy :chains chains
     :fires fires :cands cands :outs outs :gone gone'}))

(defn- crater! [craters {:keys [destroy fires]}]
  (doseq [[x y z] destroy] (explosion/crater! craters x y z 0))
  (doseq [[[x y z] st] fires] (explosion/crater! craters x y z st)))

(defn- broken-by [world]
  (fn [oid o] (hanging-system/kill-deltas world oid o nil nil)))

(defn- reached-body [world craters hurt fresh body req]
  (let [center (:center req)
        [hurt outs] (blast-deltas (broken-by world) craters hurt body
                                  center)
        fresh (fresh-blast craters fresh body center)
        affected (explosion/reached (:rays body) craters center)]
    [(assoc body :outs outs :affected affected) hurt fresh]))

(defn- request-blasts [world craters [gone primed hurt bs fresh]
                       [req body]]
  (let [rg (:rg body)
        [body hurt fresh]
        (reached-body world craters hurt fresh body req)
        b (blast-of world body req gone primed craters)
        specs (spawn-specs world rg b)
        n (count fresh)
        b (assoc b :rg rg :loaded (explosion/loaded-payloads rg)
                   :spawned [n (+ n (count specs))])]
    (crater! craters b)
    [(:gone b) (into primed (:chains b)) hurt (conj bs b)
     (into fresh specs)]))

(defn- blast-deltas-of [world fresh {:keys [rg loaded] :as b}]
  (let [acc (vec (chunks/read-absent-deltas loaded))]
    (blast-acc world rg fresh acc b)))

(defn- requests [d]
  (into [] (comp (filter (fn [delta] (= :explode (nth delta 0))))
                 (map second))
        (:world d)))

(defn- blastable [world]
  (remove (comp game-mode/spectator? val) (:entities world)))

(def ^:private ^:const job-count 12)

(defn- job-size ^long [^long n]
  (max 1 (quot (+ n (dec job-count)) job-count)))

(defn- requested-deltas [world reqs]
  (let [index (kb-index (blastable world))
        init [#{} (tnt/primed-origins world) {} [] []]
        pairs (map vector reqs (bodies world index reqs))
        step (partial request-blasts world (explosion/craters))
        [_ _ _ done fresh] (reduce step init pairs)
        blast #(blast-deltas-of world fresh %)
        jobs (partition-all (job-size (count done)))]
    (deltas/fold #(into [] (mapcat blast) %) (into [] jobs done))))

(defn blasts
  "Returns the deltas of every blast requested this tick.
  They come in request order."
  {:wake {:deltas #{:explode}}}
  [world d]
  (let [reqs (requests d)]
    (if (seq reqs)
      (requested-deltas world reqs)
      deltas/empty-deltas)))
