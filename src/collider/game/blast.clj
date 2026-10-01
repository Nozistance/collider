(ns collider.game.blast
  "One explosion, as ServerExplosion.explode:237-252: its rays, the
  bodies it reaches in section order, the blocks it breaks, their
  drops, then fire."
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.entity :as entity]
            [collider.game.entity.sections :as sections]
            [collider.game.game-mode :as game-mode]
            [collider.game.hanging :as hanging]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.systems.chunks :as chunks]
            [collider.game.systems.hanging :as hanging-system]
            [collider.game.systems.items :as items]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.fire :as fire]
            [collider.world.phys :as phys]
            [collider.world.space.explosion :as explosion]))

(set! *warn-on-reflection* true)

(defn- twice ^double [power] (double (float (* 2.0 (double power)))))

(defn- blast-damage ^double [^double k power]
  (let [d (+ 1.0 (* (/ (+ (* k k) k) 2.0) 7.0 (twice power)))]
    (double (float d))))

(def ^:private ^:const unit-least (double (float 1.0E-5)))

(defn- origin-y ^double [e p]
  (cond-> (v/y p)
    (not= :tnt (:type e)) (+ (double (entity/eye-height e)))))

(defn- impulse
  "Returns [push damage] of body e at p, d12 of twice the power away,
  that density of it sees the blast, as hurtEntities:184-200."
  [{:keys [center power]} e p d12 density]
  (let [dx (- (v/x p) (v/x center))
        dy (- (origin-y e p) (v/y center))
        dz (- (v/z p) (v/z center))
        d (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
        k (* (- 1.0 (double d12)) (double density))
        dmg (blast-damage k power)]
    (if (< d unit-least)
      [[0.0 0.0 0.0] dmg]
      [[(* (/ dx d) k) (* (/ dy d) k) (* (/ dz d) k)] dmg])))

(defn- blast-proof? [e]
  (and (= :item (:type e))
       (= "is_explosion" (data/resists (:item (:stack e))))))

(defn- hurtable? [e]
  (and (some? (:health e)) (not (blast-proof? e))))

(defn- item-dies? [e dmg]
  (and (= :item (:type e)) (hurtable? e)
       (not (pos? (double (:health (entity/hurt e (double dmg))))))))

(defn- moving? [kb]
  (not (and (zero? (double (kb 0))) (zero? (double (kb 1)))
            (zero? (double (kb 2))))))

(defn- density [{:keys [exposure center]} e]
  (let [[half height] (entity/box e)]
    (explosion/exposed exposure center (:pos e) half height
                       (phys/context e))))

(defn- pushed [b id e d12]
  (let [[kb dmg] (impulse b e (:pos e) d12 (density b e))]
    (cond
      (= :player (:type e)) {:motion (when-not (:flying e) kb)}
      (item-dies? e dmg) {:ds [[:remove-entity id]] :gone? true}
      :else {:ds (cond-> []
                   (hurtable? e) (conj [:damage id dmg (:src b)])
                   (moving? kb) (conj [:push id kb]))})))

(defn hit
  "Returns what blast b does to body e of id, d12 of twice its power
  away: :ds its deltas, :gone? when it ends, :motion the push the
  explosion packet gives a player."
  [b id e d12]
  (if (contains? hanging/types (:type e))
    (let [by (:cause (:src b))]
      {:ds (hanging-system/kill-deltas (:world b) id e (:causer b) by)
       :gone? true})
    (pushed b id e d12)))

(defn- distance [b]
  (let [c (:center b) r (twice (:power b))
        cx (v/x c) cy (v/y c) cz (v/z c)]
    (fn ^double [p]
      (let [dx (- (v/x p) cx) dy (- (v/y p) cy) dz (- (v/z p) cz)]
        (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) r)))))

(defn- reach-box [{:keys [center power]}]
  (let [r (+ (twice power) 1.0)
        f (fn [d] (mapv #(Math/floor (+ (double %) (double d)))
                        [(v/x center) (v/y center) (v/z center)]))]
    [(f (- r)) (f r)]))

(defn bodies
  "Returns [id e d12] of the bodies of index idx that blast b reaches,
  in the order of getEntities (hurtEntities:181). now gives the body
  of an id as it is, nil when it is gone."
  [b idx now]
  (let [[lo hi] (reach-box b)
        d12-of (distance b)
        f (fn [[id p]]
            (let [d12 (d12-of p)]
              (when (<= d12 1.0)
                (when-let [e (now id)]
                  (when-not (game-mode/spectator? e) [id e d12])))))]
    (into [] (keep f) (sections/within idx lo hi))))

(defn- explosion-pitch ^double [seed]
  (let [r (- (random/of-key [seed :p1]) (random/of-key [seed :p2]))]
    (* 0.7 (+ 1.0 (* 0.2 r)))))

(def ^:private decay-rules
  {:tnt :tnt-explosion-drop-decay
   :block :block-explosion-drop-decay
   :mob :mob-explosion-drop-decay})

(defn- decay-radius [world source power]
  (let [rule (get decay-rules source :block-explosion-drop-decay)]
    (when (get-in world [:rules rule] (not= :tnt source))
      power)))

(defn- interacts? [world source]
  (or (not= :mob source) (get-in world [:rules :mob-griefing] true)))

(defn- drop-specs [{:keys [world rg seed source power]} destroy]
  (when (get-in world [:rules :block-drops] true)
    (let [radius (decay-radius world source power)
          stacks (explosion/stacks rg destroy seed radius)]
      (map-indexed (fn [i [p stack]] (items/popped world p stack i))
                   stacks))))

(defn- block-at ^long [rg [x y z]]
  (explosion/read-block rg (long x) (long y) (long z)))

(defn- below ^long [rg [x y z]]
  (explosion/read-block rg (long x) (dec (long y)) (long z)))

(defn- fire-here? [rg broken p seed]
  (and (< (double (random/of-key [seed p :fire])) (/ 1.0 3.0))
       (or (contains? broken p) (zero? (block-at rg p)))
       (let [[x y z] p]
         (not (contains? broken [(long x) (dec (long y)) (long z)])))
       (block/solid-render? (below rg p))))

(defn- fire-on ^long [^long st]
  (if (block/tagged? st "soul_fire_base_blocks")
    (block/state :soul-fire)
    (fire/fire-state 0)))

(defn- fire-cells [{:keys [rg seed]} cells destroy]
  (let [broken (set destroy)]
    (into [] (keep (fn [p]
                     (when (fire-here? rg broken p seed)
                       [p (fire-on (below rg p))])))
          cells)))

(defn- broken-cells [{:keys [world rg source primed]} cells]
  (if (interacts? world source)
    (let [primed (or primed (tnt/primed-origins world))
          tnt? (get-in world [:rules :tnt-explodes] true)
          left (into [] (remove primed) cells)]
      [(if tnt? (filterv #(tnt/tnt-state? (block-at rg %)) left) [])
       left])
    [[] []]))

(defn- with-read [world rg]
  (let [absent (fn [[id _]] (not (contains? (:chunks world) id)))
        payloads (filter absent (explosion/loaded-payloads rg))]
    (update world :chunks into
            (map (fn [[id p]] [id (:chunk p)])) payloads)))

(defn- chain-spec [b p]
  (cond-> (assoc (tnt/chain-primed p (:seed b)) :origin nil)
    (:cause (:src b)) (assoc :owner (:cause (:src b)))))

(defn- reader [world center]
  (let [at (mapv #(long (double %)) center)
        read #(chunks/read-absent world %)]
    (explosion/block-reader (:chunks world) at read)))

(defn- changed-deltas [{:keys [world rg]} changes]
  (concat (chunks/read-absent-deltas (explosion/loaded-payloads rg))
          (edit/shaped-deltas (with-read world rg) changes)))

(defn- blocks
  "Returns {:ds :spawns :count} of the blocks of blast b: the deltas
  that break and burn them, the TNT they prime and the drops."
  [{:keys [rg exposure center power seed fire?] :as b}]
  (let [rays (explosion/rays rg exposure center power seed)
        reached (explosion/reached rays center)
        [chains destroy] (broken-cells b (:blocks reached))
        fires (if fire? (fire-cells b @(:cells reached) destroy) [])]
    {:ds (changed-deltas b (into (mapv (fn [p] [p 0]) destroy) fires))
     :spawns (concat (map #(chain-spec b %) chains)
                     (drop-specs b destroy))
     :count (:count reached)}))

(defn blast
  "Returns blast b of spec in world: :center, :power, :source (:tnt,
  :block or :mob), :fire?, :by the eid it goes off from, :src its
  damage source, :causer the entity that caused it and :primed the
  TNT blocks already lit, by default those of world."
  [world spec]
  (let [{:keys [center by]} spec
        rg (reader world center)]
    (assoc spec :world world :rg rg :power (double (:power spec))
           :seed [(:tick world) (or by center)]
           :exposure (explosion/exposure rg center))))

(defn finish
  "Returns {:ds :spawns} of the blocks of blast b after it hit its
  bodies, motions by player: the deltas of its blocks and its effect,
  then the bodies it spawns."
  [b motions]
  (let [{:keys [ds spawns] :as r} (blocks b)
        {:keys [center power seed]} b
        pitch (explosion-pitch seed)
        msg (out/explosion center power (:count r) motions pitch)]
    {:ds (conj (vec ds) (out/all msg)) :spawns spawns}))

(defn- motions [hits]
  (into {} (keep (fn [[id h]] (some->> (:motion h) (vector id))))
        hits))

(defn deltas
  "Returns the deltas of the blast of spec in world, every body where
  it stands."
  [world spec]
  (let [b (blast world spec)
        es (:entities world)
        hit-of (fn [[id e d12]] [id (hit b id e d12)])
        idx (sections/of (seq es))
        hits (mapv hit-of (bodies b idx #(get es %)))
        {:keys [ds spawns]} (finish b (motions hits))]
    (-> ds
        (into (mapcat (comp :ds second)) hits)
        (into (map (fn [e] [:spawn-entity e])) spawns))))
