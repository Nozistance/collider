(ns collider.game.blast
  "One explosion with its rays, the bodies it reaches, the blocks it
  breaks, their drops and its fire."
  (:require [collider.data :as data]
            [collider.game.block.tnt :as tnt]
            [collider.game.changes :as changes]
            [collider.game.delta :as delta]
            [collider.game.entity :as entity]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.sections :as sections]
            [collider.game.hanging.drops :as drops]
            [collider.game.item :as item]
            [collider.game.level :as level]
            [collider.game.hanging :as hanging]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.fire :as fire]
            [collider.world.phys :as phys]
            [collider.world.space.explosion :as explosion]))

(set! *warn-on-reflection* true)

(defn- twice ^double [power] (num/f32 (* 2.0 (double power))))

(defn- blast-damage ^double [^double k power]
  (num/f32 (+ 1.0 (* (/ (+ (* k k) k) 2.0) 7.0 (twice power)))))

(def ^:private ^:const unit-least (double (float 1.0E-5)))

(defn- origin-y ^double [e p]
  (cond-> (v/y p)
    (not= :tnt (:type e)) (+ (double (entity/eye-height e)))))

(defn- impulse
  "Returns the push and the damage the blast gives body e at p."
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

(defn- density [{:keys [exposure]} e]
  (let [[half height] (entity/box e)]
    (explosion/exposed exposure (:pos e) half height
                       (phys/context e))))

(defn- shoved?
  "Returns true when the blast moves player e.
  A flying player in creative stays where it is."
  [e]
  (not (and (game-mode/creative? e) (:flying e))))

(defn- pushed [b id e d12]
  (let [[kb dmg] (impulse b e (:pos e) d12 (density b e))]
    (cond
      (= :player (:type e))
      {:ds (hurt/damage-deltas (:world b) id e dmg (:src b))
       :motion (when (shoved? e) kb)}
      (item-dies? e dmg) {:ds [[:remove-entity id]] :gone? true}
      :else {:ds (cond-> []
                   (hurtable? e) (conj [:damage id dmg (:src b)])
                   (moving? kb) (conj [:push id kb]))})))

(defn hit
  "Returns what blast b does to body e of id, d12 of twice its power
  away. A player gets its push with the effect of the blast."
  [b id e d12]
  (if (contains? hanging/types (:type e))
    (let [by (:cause (:src b))]
      {:ds (drops/kill-deltas (:world b) id e (:causer b) by)
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
  in section order. Function now gives the body of an id as it is,
  nil when it is gone."
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
      (map-indexed (fn [i [p stack]] (item/popped world p stack i))
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
        read #(level/read-absent world %)]
    (explosion/block-reader (:chunks world) at read)))

(defn- author [{:keys [by src source]}]
  (when by
    (cond-> {:by by :with source}
      (:cause src) (assoc :owner (:cause src)))))

(defn- changed-deltas [{:keys [world rg] :as b} changes]
  (let [ds (changes/shaped-deltas (with-read world rg) changes)]
    (concat (level/read-absent-deltas (explosion/loaded-payloads rg))
            (delta/authored ds (author b)))))

(defn- blocks
  "Returns the deltas that break and burn the blocks of blast b, the
  TNT they prime, the drops and how many blocks it reached."
  [{:keys [rg exposure center power seed fire?] :as b}]
  (let [rays (explosion/rays rg exposure center power seed)
        reached (explosion/reached rays center)
        [chains destroy] (broken-cells b (:blocks reached))
        fires (if fire? (fire-cells b @(:cells reached) destroy) [])]
    {:ds (changed-deltas b (into (mapv (fn [p] [p 0]) destroy) fires))
     :spawns (concat (map #(chain-spec b %) chains)
                     (drop-specs b destroy))
     :count (:count reached)}))

(defn of
  "Returns the blast of spec in world. The spec names its centre,
  power, source, fire, damage source, the entity it goes off from,
  the entity that caused it and the TNT already lit."
  [world spec]
  (let [{:keys [center by]} spec
        rg (reader world center)]
    (assoc spec :world world :rg rg :power (double (:power spec))
           :seed [(:tick world) (or by center)]
           :exposure (explosion/exposure rg center))))

(defn finish
  "Returns the deltas of the blocks and the effect of blast b after it
  hit its bodies, with the bodies it spawns. Motions are the pushes
  of the players by eid."
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
  (let [b (of world spec)
        es (:entities world)
        hit-of (fn [[id e d12]] [id (hit b id e d12)])
        idx (sections/of (seq es))
        hits (mapv hit-of (bodies b idx #(get es %)))
        {:keys [ds spawns]} (finish b (motions hits))]
    (-> ds
        (into (mapcat (comp :ds second)) hits)
        (into (map (fn [e] [:spawn-entity e])) spawns))))
