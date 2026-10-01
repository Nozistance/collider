(ns collider.game.systems.damage
  "Damage, death and respawn."
  (:require [collider.data :as data]
            [collider.game.systems.blocks.edit :as edit]
            [collider.game.deltas :as deltas]
            [collider.game.entity :as entity]
            [collider.game.experience :as xp]
            [collider.game.game-mode :as game-mode]
            [collider.game.loot :as loot]
            [collider.random :as random]
            [collider.game.mob.mobs :as mobs]
            [collider.world.chunk :as chunk]
            [collider.game.areas :as areas]
            [collider.game.delta :as delta]
            [collider.game.player :as player]
            [collider.world.blocks.bed :as bed]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.env.weather :as weather]
            [collider.world.phys :as phys]
            [collider.game.block.menu :as menu]
            [collider.game.systems.containers :as containers]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const void-damage 4.0)

(def ^:private ^:const panic-ticks 40)

(def ^:private ^:const player-health 20.0)

(def ^:private ^:const voice-pitch-spread 0.2)

(def ^:private ^:const baby-voice-pitch 1.5)

(def ^:private ^:const adult-voice-pitch 1.0)

(def ^:private ^:const fluid-margin 0.001)

(def ^:private ^:const fire-damage-period 20)

(def ^:private ^:const ticks-per-second 20)

(defn- hurt-sound [e]
  (if (= :player (:type e))
    (cond
      (not (pos? (double (:health e)))) :player/death
      (pos? (long (or (:fire e) 0))) :player/hurt-on-fire
      :else :player/hurt)
    (if (pos? (double (:health e)))
      (mobs/sound-of e :hurt)
      (mobs/sound-of e :death))))

(defn- sound-pitch ^double [world eid e]
  (let [t (long (:tick world))
        base (if (:baby-until e) baby-voice-pitch adult-voice-pitch)
        r (- (random/of-longs t eid (hash :hurt1))
             (random/of-longs t eid (hash :hurt2)))]
    (+ base (* voice-pitch-spread r))))

(defn creative-proof?
  "Returns true when nothing can hurt the entity."
  [e]
  (= :player (:type e)))

(def ^:private ^:const fire-seconds 8)

(def ^:private ^:const lava-seconds 15)

(def ^:private ^:const lava-damage 4.0)

(defn- box-of [e] (entity/box e))

(defn- chunk-x ^long [^double a]
  (bit-shift-right (long (Math/floor a)) 4))

(defn- has-chunk? [chunks x z]
  (some? (get chunks (chunk/pos->id x z))))

(defn- loaded-near? [world e]
  (let [chunks (:chunks world) p (:pos e)
        x0 (chunk-x (- (v/x p) 0.5)) x1 (chunk-x (+ (v/x p) 0.5))
        z0 (chunk-x (- (v/z p) 0.5)) z1 (chunk-x (+ (v/z p) 0.5))]
    (or (has-chunk? chunks x0 z0)
        (and (not= x0 x1) (has-chunk? chunks x1 z0))
        (and (not= z0 z1) (has-chunk? chunks x0 z1))
        (and (not= x0 x1) (not= z0 z1)
             (has-chunk? chunks x1 z1)))))

(def ^:private ^:const sunk-shrink-xz 0.1)

(def ^:private ^:const sunk-shrink-y 0.4)

(def ^:private ^:const fire-bit 1)

(def ^:private ^:const lava-bit 2)

(def ^:private ^:const sunk-bit 4)

(defn- any-bit? [^long flags ^long mask]
  (not (zero? (bit-and flags mask))))

(defn- floor-lo ^long [^double a]
  (long (Math/floor (+ a fluid-margin))))

(defn- floor-hi ^long [^double a]
  (long (Math/floor (+ (- a fluid-margin) 1.0))))

(defn- span
  ^longs [p ^double half ^double height [sxz sy]]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        sy (double sy)
        h (- half (double sxz))]
    (doto (long-array 6)
      (aset 0 (floor-lo (- px h))) (aset 1 (floor-hi (+ px h)))
      (aset 2 (max chunk/min-y (floor-lo (+ py sy))))
      (aset 3 (min (inc chunk/max-y) (floor-hi (- (+ py height) sy))))
      (aset 4 (floor-lo (- pz h))) (aset 5 (floor-hi (+ pz h))))))

(defn- burn-bit ^long [st]
  (cond (block/lava? st) lava-bit (block/fire? st) fire-bit :else 0))

(def ^:private ^:table burn-bits
  (delay
    (let [a (byte-array (data/block-state-count))]
      (dotimes [st (alength a)] (aset a st (byte (burn-bit st))))
      a)))

(defn- probe
  (^long [world e] (probe world e (box-of e)))
  (^long [world e [half height]]
   (let [p (:pos e)
         outer (span p (double half) (double height)
                     [fluid-margin fluid-margin])
         inner (span p (double half) (double height)
                     [sunk-shrink-xz sunk-shrink-y])]
     (phys/burns (:chunks world) @burn-bits outer inner))))

(defn- burning-flag [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(def ^:private on-fire {:type :on-fire})

(def ^:private in-fire {:type :in-fire})

(def ^:private in-lava {:type :lava})

(def ^:private out-of-world {:type :out-of-world})

(defn- burn-tick-deltas [eid fire wet? in-lava?]
  (when (and (pos? fire) (not wet?))
    (let [due? (and (zero? (rem fire fire-damage-period))
                    (not in-lava?))]
      (cond-> [[:merge-entity eid {:fire (dec fire)}]]
              due? (conj [:damage eid 1.0 on-fire])))))

(defn- fire-ticks ^long [fire seconds]
  (max (long fire) (* ticks-per-second (long seconds))))

(defn- ignite-deltas [eid fire wet? damage seconds src]
  (cond-> [[:damage eid (double damage) src]]
          (not wet?)
          (conj [:merge-entity eid
                 {:fire (fire-ticks fire seconds)}])))

(defn- fizz-cell [e]
  (mapv (fn [c] (long (Math/floor (double c))))
        [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))]))

(defn- douse-deltas [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz (fizz-cell e)))])))

(defn- lit-by-deltas [eid e fire wet? flags]
  (let [touch (any-bit? flags (bit-or fire-bit lava-bit))
        sunk? (any-bit? flags sunk-bit)
        ignite (fn [d s src] (ignite-deltas eid fire wet? d s src))]
    (concat (burning-flag eid e fire sunk?)
            (when touch (ignite 1.0 fire-seconds in-fire))
            (when sunk? (ignite lava-damage lava-seconds in-lava))
            (douse-deltas eid e fire wet?))))

(defn- lit-deltas [eid e fire wet? flags]
  (when (or (pos? (long fire)) (pos? (long flags)) (:burning? e))
    (lit-by-deltas eid e fire wet? flags)))

(def ^:private ^:const fire-rest -20)

(def ^:private ^:const inside-margin 1.0E-5)

(def ^:private ^:const fluid-slack 1.0E-7)

(defn- capped ^long [^long f] (min f 1))

(defn- cell-span [^double lo ^double hi]
  (range (long (Math/floor (+ lo inside-margin)))
         (inc (long (Math/floor (- hi inside-margin))))))

(defn- inside-cells [e]
  (let [p (:pos e)
        [half height] (box-of e)
        h (double half)
        x (v/x p) y (v/y p) z (v/z p)]
    (for [cx (cell-span (- x h) (+ x h))
          cy (cell-span y (+ y (double height)))
          cz (cell-span (- z h) (+ z h))]
      [cx cy cz])))

(defn- in-fluid? [chunks e cls c]
  (when-let [top (liquid/surface chunks cls c)]
    (< (v/y (:pos e)) (- (double top) fluid-slack))))

(defn- cell-state ^long [chunks [x y z]]
  (chunk/block-state chunks x y z))

(defn- lo-cell ^long [^double a]
  (long (Math/floor (+ a inside-margin))))

(defn- hi-cell ^long [^double a]
  (long (Math/floor (- a inside-margin))))

(defn- all-air? [chunks e]
  (let [p (:pos e)
        [half height] (box-of e)
        h (double half) x (v/x p) y (v/y p) z (v/z p)
        x1 (hi-cell (+ x h)) y0 (lo-cell y)
        y1 (hi-cell (+ y (double height)))
        z0 (lo-cell (- z h)) z1 (hi-cell (+ z h))]
    (loop [cx (lo-cell (- x h)) cy y0 cz z0]
      (cond (> cx x1) true
            (> cy y1) (recur (inc cx) y0 z0)
            (> cz z1) (recur cx (inc cy) z0)
            (zero? (chunk/block-state chunks cx cy cz))
            (recur cx cy (inc cz))
            :else false))))

(def ^:private calm
  {:fire? false :lava? false :water? false :snow []})

(defn- touched [world e]
  (let [chunks (:chunks world)
        cs (inside-cells e)
        of (fn [pred] (boolean (some pred cs)))]
    {:fire? (of #(block/fire? (cell-state chunks %)))
     :lava? (of #(in-fluid? chunks e :lava %))
     :water? (of #(in-fluid? chunks e :water %))
     :snow (filterv #(= :powder-snow
                        (block/type-of (cell-state chunks %)))
                    cs)}))

(defn- contact [world e]
  (if (all-air? (:chunks world) e)
    calm
    (touched world e)))

(defn- rained-on? [world e]
  (let [p (:pos e)
        [_ height] (box-of e)
        x (long (Math/floor (v/x p)))
        z (long (Math/floor (v/z p)))
        top (long (Math/floor (+ (v/y p) (double height))))
        rain? #(weather/raining-at? world (:chunks world) %)]
    (or (rain? [x (long (Math/floor (v/y p))) z]) (rain? [x top z]))))

(defn- fire-touched ^long [^long f]
  (let [f (if (neg? f) (inc f) (capped (inc f)))]
    (if (neg? f)
      f
      (capped (max f (* ticks-per-second fire-seconds))))))

(defn- lit-by ^long [^long f c]
  (let [f (if (:fire? c) (fire-touched f) f)]
    (if (:lava? c)
      (capped (max f (* ticks-per-second lava-seconds)))
      f)))

(defn- player-fire [world e c]
  (let [f0 (long (or (:fire e) 0))
        f1 (if (pos? f0) (capped (dec f0)) f0)
        lit (lit-by f1 c)
        wet? (or (:water? c) (seq (:snow c)) (rained-on? world e))
        f (if wet? (min 0 lit) lit)]
    [f0 f1 lit (if (and (<= f 0) (<= f f1)) fire-rest f)]))

(defn- melt-effect [chunks c]
  (out/all (out/break-effect c (cell-state chunks c))))

(defn- melt-deltas [world cells]
  (when (seq cells)
    (concat (edit/set-deltas world (mapv (fn [c] [c 0]) cells))
            (map #(melt-effect (:chunks world) %) cells))))

(defn- put-out-sound [world eid e]
  (let [t (long (:tick world))
        r (- (random/of-longs t eid (hash :put-out1))
             (random/of-longs t eid (hash :put-out2)))]
    (out/all (out/sound :generic/extinguish-fire (:pos e) 0.7
                        (+ 1.6 (* 0.4 r)) :players))))

(defn- player-fire-deltas [world eid e]
  (let [c (contact world e)
        [f0 f1 lit f] (player-fire world e c)]
    (concat (burning-flag eid e f1 false)
            (when (not= f f0) [[:merge-entity eid {:fire f}]])
            (when (pos? (long lit)) (melt-deltas world (:snow c)))
            (when (and (pos? (long f1)) (<= (long f) 0))
              [(put-out-sound world eid e)]))))

(defn fire-deltas
  "Returns the deltas of the fire and lava entity eid touches,
  wet as wet? tells or as it is."
  ([world eid e] (fire-deltas world eid e (:wet? e)))
  ([world eid e wet?]
   (let [fire (long (or (:fire e) 0))]
     (cond
       (creative-proof? e) (player-fire-deltas world eid e)
       :else
       (let [[half height :as box] (box-of e)]
         (when-not (and (zero? fire) (not (:burning? e))
                        (phys/cool? (:chunks world) @burn-bits (:pos e)
                                    half height))
           (lit-deltas eid e fire (boolean wet?)
                       (probe world e box))))))))

(def ^:private ^:const burn-volume 0.4)

(def ^:private ^:const burn-pitch 2.0)

(def ^:private ^:const burn-pitch-spread 0.4)

(def ^:private ^:const burn-sound-period 10)

(defn- fire-proof-item? [e]
  (= "is_fire" (data/resists (:item (:stack e)))))

(defn- item-wet? [world e]
  (let [[half height] (box-of e)]
    (pos? (liquid/fluid-height
            (:chunks world) (:pos e) half height :water))))

(defn- item-fire-deltas [world eid e ^long flags]
  (let [fire (long (or (:fire e) 0))
        touched? (or (pos? fire) (pos? flags))
        wet? (boolean (and touched? (item-wet? world e)))
        lava? (pos? (bit-and flags lava-bit))
        ignite (fn [d s] (ignite-deltas eid fire wet? d s nil))]
    (concat (burning-flag eid e fire false)
            (burn-tick-deltas eid fire wet? lava?)
            (when (pos? (bit-and flags fire-bit))
              (ignite 1.0 fire-seconds))
            (when lava? (ignite lava-damage lava-seconds))
            (douse-deltas eid e fire wet?))))

(defn- damage-sum ^double [deltas]
  (reduce (fn [^double s d]
            (if (= :damage (nth d 0)) (+ s (double (nth d 2))) s))
          0.0 deltas))

(defn- burn-sound-deltas [world eid e ^double health]
  (when (or (<= (- health lava-damage) 0.0)
            (= :experience-orb (:type e))
            (zero? (rem (inc (long (or (:age e) 0)))
                        burn-sound-period)))
    (let [r (random/of-key (:tick world) eid :burn)
          pitch (+ burn-pitch (* burn-pitch-spread r))]
      [(out/all
         (out/sound :generic/burn (:pos e) burn-volume pitch))])))

(defn- item-burn-deltas [world eid e ^long flags]
  (let [ds (item-fire-deltas world eid e flags)
        health (double (:health e))]
    (concat ds
            (when (pos? (bit-and flags lava-bit))
              (burn-sound-deltas world eid e health))
            (when (>= (damage-sum ds) health)
              [[:remove-entity eid]]))))

(defn- unlit? [e ^long flags]
  (and (zero? flags) (not (pos? (long (or (:fire e) 0))))
       (not (:burning? e))))

(defn- item-deltas [world eid e]
  (if (fire-proof-item? e)
    (when (pos? (long (or (:fire e) 0)))
      [[:merge-entity eid {:fire 0}]])
    (let [flags (probe world e)]
      (if (unlit? e flags)
        (when (>= 0.0 (double (:health e))) [[:remove-entity eid]])
        (item-burn-deltas world eid e flags)))))

(def ^:private ^:const safe-fall 3.0)

(defn- ground-state [world pos]
  (chunk/chunks-get-block
    (:chunks world)
    [(long (Math/floor (v/x pos)))
     (long (Math/floor (- (v/y pos) 0.2)))
     (long (Math/floor (v/z pos)))]))

(defn- landing-particles [world e ^double fall]
  (let [power (Math/floor (+ (- fall safe-fall) 1.0e-6))
        pos (:pos e)
        st (ground-state world pos)]
    (when (and (pos? power) (not (block/air? st)))
      (let [scale (min (+ 0.2 (/ power 15.0)) 2.5)
            at [(v/x pos) (v/y pos) (v/z pos)]
            n (long (* 150.0 scale))]
        [(out/all (out/particles :block st at n 0.15))]))))

(defn- landing-deltas [world eid e]
  (when-let [fall (:landed e)]
    (concat [[:merge-entity eid {:landed nil}]]
            (landing-particles world e (double fall)))))

(defn- loading? [world e]
  (and (= :player (:type e))
       (not (player/client-loaded? e (inc (long (:tick world)))))))

(defn- void-deltas [world eid e]
  (when (and (pos? (double (:health e)))
             (< (v/y (:pos e)) (chunk/void-y world))
             (not (loading? world e)))
    [[:damage eid void-damage out-of-world]]))

(def ^:private ^:table panic-causes
  (delay (set (data/tag-values "damage_type" "panic_causes"))))

(defn- panics? [e]
  (let [c (:hurt-cause e)]
    (or (nil? c) (contains? @panic-causes c))))

(defn- panic-until [world e]
  (when (panics? e) (+ (long (:tick world)) panic-ticks)))

(defn- panicked [world e]
  (cond-> {:love-until nil :panic-until (panic-until world e)}
          (:hurt-cause e) (assoc :hurt-cause nil)))

(def ^:private ^:const player-kill-memory 100)

(def ^:private ^:table drop-tables (delay (data/entity-drops)))

(defn- killed-by-player? [world e]
  (let [at (:hurt-by-player e)]
    (boolean (and at (< (- (long (:tick world)) (long at))
                        player-kill-memory)))))

(defn- loot-ctx [world e]
  (let [fire (long (or (:fire e) 0))]
    {:on-fire? (or (pos? fire) (boolean (:burning? e)))
     :killed-by-player? (killed-by-player? world e)
     :damage-type nil
     :looting 0
     :entity (mobs/loot-entity e)}))

(defn- loot-stacks [world eid e]
  (loot/drops @drop-tables (:type e) (loot-ctx world e)
              #(random/of-key (:tick world) eid %)))

(defn- drops-loot?
  "LivingEntity.shouldDropLoot: not a baby, and the mob_drops rule."
  [world e]
  (and (get @drop-tables (:type e))
       (not (mobs/baby? e))
       (get-in world [:rules :mob-drops] true)))

(defn- drop-deltas [world eid e]
  (when (drops-loot? world e)
    (let [t (:tick world)
          spawn (fn [i s]
                  (let [v (entity/pop-velocity [t eid :loot i])]
                    [:spawn-entity (entity/item (:pos e) v s)]))]
      (map-indexed spawn (loot-stacks world eid e)))))

(defn- keeps? [world] (get-in world [:rules :keep-inventory] false))

(defn- mob-reward [world eid e]
  (when (and (killed-by-player? world e) (not (mobs/baby? e))
             (get-in world [:rules :mob-drops] true))
    (let [r (random/of-key (:tick world) eid :xp-reward)]
      (inc (long (* 3.0 r))))))

(defn- player-reward [world e]
  (xp/death-reward e (keeps? world) (game-mode/spectator? e)))

(defn- death-orbs
  "Returns the orbs LivingEntity.dropExperience leaves where e died.
  Players drop some of their levels, animals a few points when a
  player killed them."
  [world eid e]
  (let [n (if (= :player (:type e))
            (player-reward world e)
            (mob-reward world eid e))]
    (when (and n (pos? (long n)))
      [[:xp-award (vec (:pos e)) (long n) [:death eid]]])))

(defn- hurt-marks [world e ^double health src]
  (cond-> {:health-sent health}
          src (assoc :struck-by nil)
          (not= :player (:type e)) (merge (panicked world e))))

(defn- struck-deltas
  "Returns the effects of a full hit from src on entity eid, as
  LivingEntity.hurtServer:1247-1266 broadcasts the damage event to
  its viewers and itself and plays the hurt sound, or the death
  sound when it killed."
  [world eid e src]
  (let [snd (hurt-sound e)
        ev (out/damage-event eid (:type src) (:cause src)
                             (:direct src) (:pos src))]
    (cond-> [(out/all ev)]
      (= :player (:type e)) (conj (out/to eid ev))
      snd (conj (out/all (out/sound snd (:pos e) 1.0
                                    (sound-pitch world eid e)))))))

(defn- died-deltas [world eid e]
  (concat [(out/all (out/status eid :death))]
          (drop-deltas world eid e) (death-orbs world eid e)))

(defn report-deltas
  "Returns the deltas that show the hurts of entity eid since they
  were last shown: the effects of its full hit, and on death its
  drops."
  [world eid e]
  (let [health (double (:health e))
        shown (double (or (:health-sent e) health))
        src (:struck-by e)
        lost? (< health shown)]
    (when (or src lost?)
      (concat
        [[:merge-entity eid (hurt-marks world e health src)]]
        (when src (struck-deltas world eid e src))
        (when (and lost? (not (pos? health)))
          (died-deltas world eid e))
        (when (and lost? (= :player (:type e)))
          [(out/to eid (out/health health))])))))

(defn timer-deltas
  "Returns the deltas of LivingEntity.tickDeath for entity eid."
  [eid e]
  (let [dead? (not (pos? (double (:health e))))
        death (when dead? (inc (long (or (:death-time e) 0))))
        gone? (and death (>= (long death) mobs/death-ticks)
                   (not= :player (:type e)))]
    (concat
      (when death [[:merge-entity eid {:death-time death}]])
      (when gone? [[:remove-entity eid]]))))

(defn respawn-config
  "Returns the respawn point player e set, or nil when it set none."
  [e]
  (if-let [{:keys [pos yaw pitch]} (:forced-spawn e)]
    {:pos     pos :yaw (double (or yaw 0.0))
     :pitch   (double (or pitch 0.0)) :forced? true}
    (when-let [pos (:spawn e)]
      {:pos pos :yaw (:yaw e 0.0) :pitch 0.0 :forced? false})))

(def ^:private ^:const stand-up-reach 3)

(defn- reach-chunks [c]
  (let [c (long c)]
    (range (bit-shift-right (- c stand-up-reach) 4)
           (inc (bit-shift-right (+ c stand-up-reach) 4)))))

(defn respawn-chunk-ids
  "Returns the ids of the chunks the check of the respawn point of
  player e reads."
  [e]
  (when-let [{[x _ z] :pos} (respawn-config e)]
    (for [cx (reach-chunks x) cz (reach-chunks z)]
      (chunk/pos->id cx cz))))

(defn- respawnable? [chunks pos]
  (block/possible-to-respawn-in?
    (chunk/chunks-get-block chunks pos)))

(defn- free-to-stand? [chunks [x y z]]
  (and (respawnable? chunks [x y z])
       (respawnable? chunks [x (inc (long y)) z])))

(defn- stand-on [pos yaw pitch]
  [[(+ (double (nth pos 0)) 0.5)
    (+ (double (nth pos 1)) 0.1)
    (+ (double (nth pos 2)) 0.5)]
   yaw pitch])

(defn- found-respawn [chunks {:keys [pos yaw pitch forced?]}]
  (if (bed/head-pos chunks pos)
    (let [up (bed/stand-up-position chunks pos yaw)]
      [up (bed/look-yaw pos up) 0.0])
    (when (and forced? (free-to-stand? chunks pos))
      (stand-on pos yaw pitch))))

(defn bed-respawn
  "Returns [pos yaw pitch] at the respawn point of player e.
  Returns nil when it has none or the point cannot be used."
  [chunks e]
  (when-let [cfg (respawn-config e)]
    (found-respawn chunks cfg)))

(defn- reshow-deltas [world eid]
  (for [[oid o] (:entities world)
        :when (contains? (:tracking o) eid)]
    [:tracking oid [] [eid]]))

(def ^:private no-experience
  {:xp-level 0 :xp-progress 0.0 :xp-total 0 :score 0})

(defn- fresh-marks [e tick keep?]
  (cond-> {:health player-health :health-sent player-health
           :hurt-resist 0 :last-damage 0.0 :death-time 0
           :born tick :ambience nil :xp-sent -1 :level-up-at 0
           :xp-ready-at nil :client-vel [0.0 0.0 0.0]}
    (not keep?) (merge no-experience)
    (seq (:effects e)) (assoc :effects {})
    (:absorption e) (assoc :absorption nil)))

(defn- shown-experience [e keep?]
  (let [e (if keep? e (merge e no-experience))]
    (out/experience (:xp-progress e 0.0) (:xp-level e 0)
                    (:xp-total e 0))))

(defn- revived [eid e pos yaw pitch tick keep?]
  [[:teleport eid pos]
   [:merge-entity eid (fresh-marks e tick keep?)]
   (out/to eid (out/respawn))
   (out/to eid (out/teleport pos yaw pitch))
   (out/to eid (shown-experience e keep?))
   (out/to eid (out/health player-health))
   (out/to eid (out/held-slot (long (or (:held-slot e) 0))))])

(defn- own-slots [inv]
  (out/inventory (mapv inv (range menu/slot-count)) nil))

(def ^:private not-valid
  (out/overlay {:translate "block.minecraft.spawn.not_valid"}))

(defn- kept-xp? [world e]
  (or (keeps? world) (game-mode/spectator? e)))

(defn respawn-deltas
  "Returns the deltas that bring dead player eid back at pos.
  When lost? is true they tell it the respawn point it set was lost."
  [world eid [pos yaw pitch lost?]]
  (let [e (get-in world [:entities eid])
        inv (apply dissoc (:inventory e) (range 5))
        t (:tick world)
        back (revived eid e pos yaw pitch t (kept-xp? world e))]
    (cond-> (into (vec (containers/removed-deltas world eid e)) back)
      (seq inv) (conj (out/to eid (own-slots inv)))
      lost? (conj (out/to eid not-valid))
      true (into (reshow-deltas world eid)))))

(defn- idle? [world e]
  (let [health (double (or (:health e) 0.0))]
    (and (pos? health)
         (>= (v/y (:pos e)) (chunk/void-y world))
         (not (pos? (long (or (:fire e) 0))))
         (not (:burning? e))
         (zero? (long (or (:hurt-resist e) 0)))
         (nil? (:landed e))
         (nil? (:struck-by e))
         (>= health (double (or (:health-sent e) health))))))

(defn- own-apply [world eid e d]
  (if-let [g (and (= eid (nth d 1 nil))
                  (get delta/entity-apply (nth d 0)))]
    (g (:tick world) e d)
    e))

(defn hurt-now
  "Returns entity eid after its own deltas of ds."
  [world eid e ds]
  (reduce (fn [e' d] (own-apply world eid e' d)) e ds))

(defn- burn-deltas [world eid e]
  (let [fire (long (or (:fire e) 0))]
    (when (and (pos? fire) (not (creative-proof? e)))
      (burn-tick-deltas eid fire (boolean (:wet? e))
                        (any-bit? (probe world e) lava-bit)))))

(defn base-deltas
  "Returns the deltas of living entity eid that LivingEntity.baseTick
  makes before the countdown of its hurt resistance: the fire it
  burns in (Entity.baseTick:546-556), then the void
  (Entity.checkBelowWorld:564), then the countdown (:483)."
  [world eid e]
  (-> (vec (burn-deltas world eid e))
      (into (void-deltas world eid e))
      (conj [:rest eid])))

(defn- busy-deltas [world eid e]
  (let [ds (concat (timer-deltas eid e)
                   (landing-deltas world eid e)
                   (fire-deltas world eid e))]
    (->> (concat (base-deltas world eid e) ds)
         (hurt-now world eid e)
         (report-deltas world eid)
         (concat ds))))

(defn- mob-deltas [world eid e]
  (if (idle? world e)
    (fire-deltas world eid e)
    (busy-deltas world eid e)))

(defn- living-deltas [world eid e]
  (if (contains? #{:item :experience-orb} (:type e))
    (item-deltas world eid e)
    (mob-deltas world eid e)))

(defn- stirred? [world e]
  (if (creative-proof? e)
    (loaded-near? world e)
    (pos? (probe world e))))

(defn- live? [world [_ e]]
  (and (some? (:health e))
       (or (not (idle? world e)) (stirred? world e))))

(defn- ticking? [active e]
  (or (= :player (:type e)) (areas/active-at? active (:pos e))))

(defn- living [world]
  (let [active (areas/active-chunks world)
        due? (fn [[_ e :as entry]]
               (and (ticking? active e)
                    (not (mobs/mob-type? (:type e)))
                    (live? world entry)))]
    (comp (filter due?)
          (mapcat (fn [[eid e]] (living-deltas world eid e))))))

(def ^:private ^:const living-leaf 64)

(defn based?
  "Returns true when living entity e has work in the start of its
  base tick: hurt resistance, fire or the void."
  [world e]
  (or (pos? (long (or (:hurt-resist e) 0)))
      (pos? (long (or (:fire e) 0)))
      (and (:health e)
           (< (v/y (:pos e)) (chunk/void-y world)))))

(defn- own-tick? [e]
  (let [k (:type e)]
    (or (mobs/mob-type? k) (contains? #{:item :experience-orb} k))))

(defn countdown
  "Returns the deltas of every player from the start of
  LivingEntity.baseTick: fire, void, then the countdown of its hurt
  resistance. A mob counts in its own turn."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [active (areas/active-chunks world)
        due? (fn [[_ e]]
               (and (ticking? active e) (not (own-tick? e))
                    (based? world e)))
        xf (comp (filter due?)
                 (mapcat (fn [[eid e]] (base-deltas world eid e))))]
    (deltas/of-vec (deltas/select xf (:entities world)))))

(defn damage
  "Returns the deltas of every player and item this tick.
  A mob takes its own in its turn."
  {:wake {:keys [:entities]}}
  [world _d]
  (deltas/of-vec
    (deltas/select (living world) (:entities world) living-leaf)))
