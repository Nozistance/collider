(ns collider.game.systems.damage
  "Damage, death and respawn."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.loot :as loot]
            [collider.random :as random]
            [collider.game.mob.mobs :as mobs]
            [collider.world.chunk :as chunk]
            [collider.game.state :as state]
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

(def ^:private ^:const death-ticks 20)

(def ^:private ^:const panic-ticks 40)

(def ^:private ^:const player-health 20.0)

(def ^:private ^:const reach-sq 36.0)

(def ^:private ^:const blind-reach-sq 9.0)

(def ^:private ^:const base-damage 1.0)

(def ^:private ^:const crit-multiplier 1.5)

(def ^:private ^:const knockback-attack-strength 0.5)

(def ^:private ^:const knockback-lift 0.1)

(def ^:private ^:const voice-pitch-spread 0.2)

(def ^:private ^:const baby-voice-pitch 1.5)

(def ^:private ^:const adult-voice-pitch 1.0)

(def ^:private ^:const fluid-margin 0.001)

(def ^:private ^:const fire-damage-period 20)

(def ^:private ^:const ticks-per-second 20)

(def ^:private ^:const player-half 0.3)

(def ^:private ^:const player-height 1.8)

(defn- weapon-damage ^double [item]
  (double (get-in (data/items) [item :attack-damage] 0.0)))

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
  "Returns true when nothing can hurt the entity.
  Players are always in creative mode."
  [e]
  (= :player (:type e)))

(defn- held-item [e]
  (:item (get (:inventory e) (+ 36 (long (or (:held-slot e) 0))))))

(defn- eye-of [e]
  (let [p (:pos e)]
    [(v/x p) (+ (v/y p) (entity/eye-height e)) (v/z p)]))

(defn- ray-steps ^long [[ax ay az] [bx by bz]]
  (let [dx (- (double bx) (double ax))
        dy (- (double by) (double ay))
        dz (- (double bz) (double az))
        d (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))]
    (long (Math/ceil (/ d 0.1)))))

(defn- lerp-floor ^long [^double a ^double b ^double s]
  (long (Math/floor (+ a (* (- b a) s)))))

(defn- ray-cell [[ax ay az] [bx by bz] ^double s]
  [(lerp-floor (double ax) (double bx) s)
   (lerp-floor (double ay) (double by) s)
   (lerp-floor (double az) (double bz) s)])

(defn- clear-ray? [chunks a b ^long n]
  (loop [i 1 lx Long/MIN_VALUE ly Long/MIN_VALUE lz Long/MIN_VALUE]
    (if (>= i n)
      true
      (let [[x y z] (ray-cell a b (/ (double i) n))
            x (long x) y (long y) z (long z)]
        (if (and (= x lx) (= y ly) (= z lz))
          (recur (inc i) x y z)
          (if (phys/solid? chunks x y z)
            false
            (recur (inc i) x y z)))))))

(defn- sees? [world a b]
  (let [pa (eye-of a) pb (eye-of b)]
    (clear-ray? (:chunks world) pa pb (ray-steps pa pb))))

(defn- attackable? [a t]
  (and a t (:health t) (pos? (double (:health t)))
       (not (creative-proof? t))))

(defn- in-reach? [world a t]
  (let [d2 (v/dist3-sq (:pos a) (:pos t))]
    (cond (< d2 blind-reach-sq) true
          (>= d2 reach-sq) false
          :else (sees? world a t))))

(defn- crit? [a t]
  (and (not (:on-ground a))
       (neg? (v/y (or (:client-vel a) [0.0 0.0 0.0])))
       (not (:wet? t))))

(defn- melee-damage ^double [a crit?]
  (cond-> (+ base-damage (weapon-damage (held-item a)))
          crit? (* crit-multiplier)))

(defn- sprint-push [a target]
  (let [yaw (Math/toRadians (double (:yaw a)))
        k knockback-attack-strength]
    [:push target [(* (- (Math/sin yaw)) k) knockback-lift
                   (* (Math/cos yaw) k)]]))

(defn- hit-marks [a tick]
  (cond-> {:love-until nil}
          (= :player (:type a)) (assoc :hurt-by-player tick)))

(defn- hit-deltas [a t target crit? tick]
  (cond-> [[:damage target (melee-damage a crit?)
            (- (v/x (:pos a)) (v/x (:pos t)))
            (- (v/z (:pos a)) (v/z (:pos t)))]
           [:merge-entity target (hit-marks a tick)]]
          (:sprinting? a) (conj (sprint-push a target))
          crit? (into [(out/all (out/animation target :crit))])))

(defn- attack-deltas [world [_ eid target]]
  (let [a (get-in world [:entities eid])
        t (get-in world [:entities target])]
    (when (and (attackable? a t) (in-reach? world a t))
      (hit-deltas a t target (crit? a t) (:tick world)))))

(def ^:private ^:const fire-seconds 8)

(def ^:private ^:const lava-seconds 15)

(def ^:private ^:const lava-damage 4.0)

(def ^:private ^:const item-half 0.125)

(def ^:private ^:const item-height 0.25)

(defn- box-of [e]
  (case (:type e)
    :player [player-half player-height]
    :item [item-half item-height]
    (mobs/box-of e)))

(defn- chunk-x ^long [^double a]
  (bit-shift-right (long (Math/floor a)) 4))

(defn- has-chunk? [chunks x z]
  (some? (get chunks (chunk/pos->id x z))))

(defn- near-edits?
  "Returns true when the entity stands in a chunk the world holds."
  [world e]
  (let [chunks (:chunks world)]
    (when (seq chunks)
      (let [p (:pos e)
            x0 (chunk-x (- (v/x p) 0.5)) x1 (chunk-x (+ (v/x p) 0.5))
            z0 (chunk-x (- (v/z p) 0.5)) z1 (chunk-x (+ (v/z p) 0.5))]
        (or (has-chunk? chunks x0 z0)
            (and (not= x0 x1) (has-chunk? chunks x1 z0))
            (and (not= z0 z1) (has-chunk? chunks x0 z1))
            (and (not= x0 x1) (not= z0 z1)
                 (has-chunk? chunks x1 z1)))))))

(def ^:private ^:const sunk-shrink-xz 0.1)

(def ^:private ^:const sunk-shrink-y 0.4)

(def ^:private ^:const fire-bit 1)

(def ^:private ^:const lava-bit 2)

(def ^:private ^:const sunk-bit 4)

(def ^:private ^:const all-bits 7)

(defn- any-bit? [^long flags ^long mask]
  (not (zero? (bit-and flags mask))))

(defn- floor-lo ^long [^double a]
  (long (Math/floor (+ a fluid-margin))))

(defn- floor-hi ^long [^double a]
  (long (Math/floor (+ (- a fluid-margin) 1.0))))

(defn- span
  "Returns the blocks the body of an entity reaches into.
  The body is shrunk by the given margins."
  [p ^double half ^double height [sxz sy]]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        sy (double sy)
        h (- half (double sxz))]
    [(floor-lo (- px h)) (floor-hi (+ px h))
     (max chunk/min-y (floor-lo (+ py sy)))
     (min (inc chunk/max-y) (floor-hi (- (+ py height) sy)))
     (floor-lo (- pz h)) (floor-hi (+ pz h))]))

(defn- mark-cell [chunks ^longs acc x y z inner?]
  (let [st (chunk/block-state chunks x y z)
        lava? (block/lava? st)]
    (when lava? (aset acc 0 (bit-or (aget acc 0) lava-bit)))
    (when (block/fire? st)
      (aset acc 0 (bit-or (aget acc 0) fire-bit)))
    (when (and lava? inner?)
      (aset acc 0 (bit-or (aget acc 0) sunk-bit)))))

(defn- scan-z [chunks ^longs acc outer inner x y yin?]
  (let [z1 (outer 5) iz0 (inner 4) iz1 (inner 5)]
    (loop [z (outer 4)]
      (when (and (< z z1) (not= all-bits (aget acc 0)))
        (mark-cell chunks acc x y z
                   (and yin? (>= z iz0) (< z iz1)))
        (recur (inc z))))))

(defn- scan-y [chunks ^longs acc outer inner x xin?]
  (let [y1 (outer 3) iy0 (inner 2) iy1 (inner 3)]
    (loop [y (outer 2)]
      (when (and (< y y1) (not= all-bits (aget acc 0)))
        (scan-z chunks acc outer inner x y
                (and xin? (>= y iy0) (< y iy1)))
        (recur (inc y))))))

(defn- scan-x [chunks ^longs acc outer inner]
  (let [x1 (outer 1) ix0 (inner 0) ix1 (inner 1)]
    (loop [x (outer 0)]
      (when (and (< x x1) (not= all-bits (aget acc 0)))
        (scan-y chunks acc outer inner x
                (and (>= x ix0) (< x ix1)))
        (recur (inc x))))))

(defn- probe
  "Returns the touch and sunk bits for an entity in fire or lava."
  ^long [world e]
  (let [[half height] (box-of e)
        p (:pos e)
        outer (span p (double half) (double height)
                    [fluid-margin fluid-margin])
        inner (span p (double half) (double height)
                    [sunk-shrink-xz sunk-shrink-y])
        ^longs acc (long-array 1)]
    (scan-x (:chunks world) acc outer inner)
    (aget acc 0)))

(defn- burning-flag [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(defn- burn-tick-deltas
  "Counts the fire down and hurts every twentieth tick.
  Lava hurts by itself, so there the fire tick only counts."
  [eid ^long fire wet? in-lava?]
  (when (and (pos? fire) (not wet?))
    (cond-> [[:merge-entity eid {:fire (dec fire)}]]
            (and (zero? (rem fire fire-damage-period)) (not in-lava?))
            (conj [:damage eid 1.0]))))

(defn- fire-ticks ^long [fire seconds]
  (max (long fire) (* ticks-per-second (long seconds))))

(defn- ignite-deltas [eid fire wet? damage seconds]
  (cond-> [[:damage eid (double damage)]]
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

(defn- lit-deltas [eid e fire wet? flags]
  (let [touch (any-bit? flags (bit-or fire-bit lava-bit))
        sunk? (any-bit? flags sunk-bit)
        in-lava? (any-bit? flags lava-bit)]
    (concat (burning-flag eid e fire sunk?)
            (burn-tick-deltas eid fire wet? in-lava?)
            (when touch
              (ignite-deltas eid fire wet? 1.0 fire-seconds))
            (when sunk?
              (ignite-deltas eid fire wet? lava-damage lava-seconds))
            (douse-deltas eid e fire wet?))))

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

(defn- contact [world e]
  (let [chunks (:chunks world)
        cs (inside-cells e)
        of (fn [pred] (boolean (some pred cs)))]
    {:fire? (of #(block/fire? (cell-state chunks %)))
     :lava? (of #(in-fluid? chunks e :lava %))
     :water? (of #(in-fluid? chunks e :water %))
     :snow (filterv #(= :powder-snow
                        (block/type-of (cell-state chunks %)))
                    cs)}))

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
    (cons [:set-blocks (mapv (fn [c] [c 0]) cells)]
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

(defn- fire-deltas [world eid e]
  (if (creative-proof? e)
    (player-fire-deltas world eid e)
    (lit-deltas eid e (long (or (:fire e) 0)) (boolean (:wet? e))
                (probe world e))))

(def ^:private ^:const burn-volume 0.4)

(def ^:private ^:const burn-pitch 2.0)

(def ^:private ^:const burn-pitch-spread 0.4)

(def ^:private ^:const burn-sound-period 10)

(defn- fire-proof-item?
  "Returns true when the stack shrugs fire off.
  Netherite gear does."
  [e]
  (= "is_fire" (data/resists (:item (:stack e)))))

(defn- item-wet? [world e]
  (pos? (liquid/fluid-height
          (:chunks world) (:pos e) item-half item-height :water)))

(defn- item-fire-deltas [world eid e ^long flags]
  (let [fire (long (or (:fire e) 0))
        touched? (or (pos? fire) (pos? flags))
        wet? (boolean (and touched? (item-wet? world e)))
        lava? (pos? (bit-and flags lava-bit))]
    (concat (burning-flag eid e fire false)
            (burn-tick-deltas eid fire wet? lava?)
            (when (pos? (bit-and flags fire-bit))
              (ignite-deltas eid fire wet? 1.0 fire-seconds))
            (when lava?
              (ignite-deltas eid fire wet? lava-damage lava-seconds))
            (douse-deltas eid e fire wet?))))

(defn- damage-sum ^double [deltas]
  (reduce (fn [^double s d]
            (if (= :damage (nth d 0)) (+ s (double (nth d 2))) s))
          0.0 deltas))

(defn- burn-sound-deltas
  "Returns the lava burn sound of an item.
  It plays on the tick the item dies and on every tenth tick
  of its age."
  [world eid e ^double health]
  (when (or (<= (- health lava-damage) 0.0)
            (zero? (rem (inc (long (or (:age e) 0)))
                        burn-sound-period)))
    (let [r (random/of-key (:tick world) eid :burn)
          pitch (+ burn-pitch (* burn-pitch-spread r))]
      [(out/all
         (out/sound :generic/burn (:pos e) burn-volume pitch))])))

(defn- item-deltas [world eid e]
  (if (fire-proof-item? e)
    (when (pos? (long (or (:fire e) 0)))
      [[:merge-entity eid {:fire 0}]])
    (let [flags (probe world e)
          ds (item-fire-deltas world eid e flags)
          health (double (:health e))]
      (concat ds
              (when (pos? (bit-and flags lava-bit))
                (burn-sound-deltas world eid e health))
              (when (>= (damage-sum ds) health)
                [[:remove-entity eid]])))))

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

(defn- loading?
  "Tells whether player e is not hurt yet: its client has not loaded
  as of the end of its own tick."
  [world e]
  (and (= :player (:type e))
       (not (state/client-loaded? e (inc (long (:tick world)))))))

(defn- void-deltas [world eid e]
  (when (and (pos? (double (:health e)))
             (< (v/y (:pos e)) (chunk/void-y world))
             (not (loading? world e)))
    [[:damage eid void-damage]]))

(defn- panicked [world e]
  (cond-> {:love-until nil :no-action 0}
          (>= (v/y (:pos e)) (chunk/void-y world))
          (assoc :panic-until (+ (long (:tick world)) panic-ticks))))

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

(defn- drop-deltas
  "Returns the items mob e leaves where it died."
  [world eid e]
  (when (get @drop-tables (:type e))
    (let [t (:tick world)
          spawn (fn [i s]
                  (let [v (entity/pop-velocity [t eid :loot i])]
                    [:spawn-entity (entity/item (:pos e) v s)]))]
      (map-indexed spawn (loot-stacks world eid e)))))

(defn- hurt-marks [world e ^double health]
  (cond-> {:health-sent health}
          (not= :player (:type e)) (merge (panicked world e))))

(defn- report-deltas [world eid e]
  (let [health (double (:health e))
        shown (double (or (:health-sent e) health))]
    (when (< health shown)
      (concat
        [[:merge-entity eid (hurt-marks world e health)]]
        (when-let [snd (hurt-sound e)]
          (let [p (sound-pitch world eid e)]
            [(out/all (out/sound snd (:pos e) 1.0 p))]))
        [(out/all (out/status eid (if (pos? health) :hurt :death)))]
        (when-not (pos? health) (drop-deltas world eid e))
        (when (= :player (:type e))
          [(out/to eid (out/health health))])))))

(defn- timer-marks [^long resist death]
  (cond-> {}
          (pos? resist) (assoc :hurt-resist (dec resist))
          death (assoc :death-time death)))

(defn- timer-deltas [eid e]
  (let [resist (long (or (:hurt-resist e) 0))
        dead? (not (pos? (double (:health e))))
        death (when dead? (inc (long (or (:death-time e) 0))))
        gone? (and death (>= (long death) death-ticks)
                   (not= :player (:type e)))]
    (concat
      (when (or (pos? resist) death)
        [[:merge-entity eid (timer-marks resist death)]])
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
  "Returns the ids of the chunks the respawn check reads.
  The check is of the respawn point of player e."
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

(defn- revived [eid e pos yaw pitch]
  [[:teleport eid pos]
   [:merge-entity eid {:health      player-health
                       :health-sent player-health
                       :hurt-resist 0 :last-damage 0.0 :death-time 0}]
   (out/to eid (out/respawn))
   (out/to eid (out/teleport pos yaw pitch))
   (out/to eid (out/health player-health))
   (out/to eid (out/held-slot (long (or (:held-slot e) 0))))])

(defn- own-slots [inv]
  (out/inventory (mapv inv (range menu/slot-count)) nil))

(def ^:private not-valid
  (out/overlay {:translate "block.minecraft.spawn.not_valid"}))

(defn respawn-deltas
  "Returns the deltas that bring dead player eid back at pos.
  When lost? is true they tell it the respawn point it set
  was lost."
  [world eid [pos yaw pitch lost?]]
  (let [e (get-in world [:entities eid])
        inv (apply dissoc (:inventory e) (range 5))]
    (cond-> (into (vec (containers/removed-deltas world eid e))
                  (revived eid e pos yaw pitch))
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
         (>= health (double (or (:health-sent e) health))))))

(defn- own-apply [world eid e d]
  (if-let [g (and (= eid (nth d 1 nil))
                  (get state/entity-apply (nth d 0)))]
    (g (:tick world) e d)
    e))

(defn- hurt-now
  "Returns e after the deltas its own tick lays on it.
  Death is reported in the tick it happens, not the next one."
  [world eid e ds]
  (reduce (fn [e' d] (own-apply world eid e' d)) e ds))

(defn- mob-deltas [world eid e]
  (let [busy? (not (idle? world e))
        ds (concat (when busy? (timer-deltas eid e))
                   (when busy? (void-deltas world eid e))
                   (when busy? (landing-deltas world eid e))
                   (fire-deltas world eid e))]
    (concat ds (when busy?
                 (report-deltas
                   world eid (hurt-now world eid e ds))))))

(defn- living-deltas [world eid e]
  (if (= :item (:type e))
    (item-deltas world eid e)
    (mob-deltas world eid e)))

(defn- live? [world [_ e]]
  (and (some? (:health e))
       (or (not (idle? world e)) (near-edits? world e))))

(defn- ticking?
  "Tells whether entity e runs its tick: a player always, any other
  only in a chunk that runs entity ticks."
  [active e]
  (or (= :player (:type e)) (state/active-at? active (:pos e))))

(defn- live-entries [world]
  (let [active (state/active-chunks world)]
    (into [] (filter (fn [[_ e :as entry]]
                       (and (ticking? active e) (live? world entry))))
          (:entities world))))

(defn- living-batch-fn [world batch]
  #(into [] (mapcat (fn [[eid e]] (living-deltas world eid e)))
         batch))

(defn- living-fns [world]
  (mapv (fn [batch] (living-batch-fn world batch))
        (partition-all 32 (live-entries world))))

(defn- event-deltas [world events]
  (into []
        (mapcat (fn [[tag :as ev]]
                  (when (= :attack tag) (attack-deltas world ev))))
        events))

(defn damage
  "Returns a step for every living entity and the damage events.
  The events are those of this tick."
  [world d]
  (let [events (:input d)]
    (conj (living-fns world) #(event-deltas world events))))
