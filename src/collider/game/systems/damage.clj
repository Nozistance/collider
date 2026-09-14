(ns collider.game.systems.damage
  "Damage: attacks, fire, the void, falling, dying and respawning."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.random :as random]
            [collider.game.mob.mobs :as mobs]
            [collider.world.chunk :as chunk]
            [collider.game.state :as state]
            [collider.world.blocks.bed :as bed]
            [collider.world.block :as block]
            [collider.world.gen :as gen]
            [collider.world.blocks.liquid :as liquid]
            [collider.world.phys :as phys]
            [collider.game.block.menu :as menu]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const void-y (- chunk/min-y 64.0))
(def ^:private ^:const void-damage 4.0)
(def ^:private ^:const death-ticks 20)
(def ^:private ^:const panic-ticks 100)
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

(defn- weapon-damage
  "Returns the damage an item adds to a blow."
  ^double [item]
  (double (get-in (data/items) [item :attack-damage] 0.0)))

(defn- hurt-sound
  "Returns the sound an entity makes when it is hurt or killed."
  [e]
  (if (= :player (:type e))
    (cond
      (not (pos? (double (:health e)))) :player/death
      (pos? (long (or (:fire e) 0))) :player/hurt-on-fire
      :else :player/hurt)
    (mobs/say-sound (:type e))))

(defn- sound-pitch
  "Returns the pitch of an entity's voice."
  ^double [world eid e]
  (let [t (long (:tick world))
        base (if (:baby-until e) baby-voice-pitch adult-voice-pitch)
        r (- (random/of-longs t eid (hash :hurt1)) (random/of-longs t eid (hash :hurt2)))]
    (+ base (* voice-pitch-spread r))))

(defn- creative-proof?
  "Returns true when nothing in the world can hurt this entity."
  [e]
  (= :player (:type e)))

(defn- held-item
  "Returns the item in an entity's hand."
  [e]
  (:item (get (:inventory e) (+ 36 (long (or (:held-slot e) 0))))))

(defn- dist-sq
  "Returns the squared distance between two positions."
  ^double [a b]
  (let [dx (- (v/x a) (v/x b)) dy (- (v/y a) (v/y b)) dz (- (v/z a) (v/z b))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- eye-of
  "Returns where an entity looks from."
  [e]
  (let [p (:pos e)]
    [(v/x p) (+ (v/y p) (entity/eye-height e)) (v/z p)]))

(defn- ray-steps
  "Returns how finely a line between two points should be walked."
  ^long [[ax ay az] [bx by bz]]
  (let [dx (- (double bx) (double ax))
        dy (- (double by) (double ay))
        dz (- (double bz) (double az))]
    (long (Math/ceil (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) 0.1)))))

(defn- ray-cell
  "Returns the block a fraction of the way along a line."
  [[ax ay az] [bx by bz] ^double s]
  [(long (Math/floor (+ (double ax) (* (- (double bx) (double ax)) s))))
   (long (Math/floor (+ (double ay) (* (- (double by) (double ay)) s))))
   (long (Math/floor (+ (double az) (* (- (double bz) (double az)) s))))])

(defn- clear-ray?
  "Returns true when nothing solid stands between two points."
  [chunks a b ^long n]
  (loop [i 1 lx Long/MIN_VALUE ly Long/MIN_VALUE lz Long/MIN_VALUE]
    (if (>= i n)
      true
      (let [[x y z] (ray-cell a b (/ (double i) n))
            x (long x) y (long y) z (long z)]
        (if (and (= x lx) (= y ly) (= z lz))
          (recur (inc i) x y z)
          (if (phys/solid? chunks (gen/flat-chunk) x y z)
            false
            (recur (inc i) x y z)))))))

(defn- sees?
  "Returns true when one entity can see another."
  [world a b]
  (let [pa (eye-of a) pb (eye-of b)]
    (clear-ray? (:chunks world) pa pb (ray-steps pa pb))))

(defn- attackable?
  "Returns true when a target can be hit."
  [a t]
  (and a t (:health t) (pos? (double (:health t))) (not (creative-proof? t))))

(defn- in-reach?
  "Returns true when an attacker is close enough to hit a target."
  [world a t]
  (let [d2 (dist-sq (:pos a) (:pos t))]
    (cond (< d2 blind-reach-sq) true
          (>= d2 reach-sq) false
          :else (sees? world a t))))

(defn- crit?
  "Returns true when a blow lands as a critical hit."
  [a t]
  (and (not (:on-ground a))
       (neg? (v/y (or (:client-vel a) [0.0 0.0 0.0])))
       (not (:wet? t))))

(defn- melee-damage
  "Returns the damage one blow deals."
  ^double [a crit?]
  (cond-> (+ base-damage (weapon-damage (held-item a)))
          crit? (* crit-multiplier)))

(defn- sprint-push
  "Returns the delta that shoves a target hit by a sprinting attacker."
  [a target]
  (let [yaw (Math/toRadians (double (:yaw a)))]
    [:push target [(* (- (Math/sin yaw)) knockback-attack-strength) knockback-lift
                   (* (Math/cos yaw) knockback-attack-strength)]]))

(defn- hit-deltas
  "Returns the deltas for one blow landing."
  [a t target crit?]
  (cond-> [[:damage target (melee-damage a crit?)
            (- (v/x (:pos a)) (v/x (:pos t)))
            (- (v/z (:pos a)) (v/z (:pos t)))]
           [:merge-entity target {:love-until nil}]]
          (:sprinting? a) (conj (sprint-push a target))
          crit? (into [(out/all (out/animation target :crit))])))

(defn- attack-deltas
  "Returns the deltas for a player striking something."
  [world [_ eid target]]
  (let [a (get-in world [:entities eid])
        t (get-in world [:entities target])]
    (when (and (attackable? a t) (in-reach? world a t))
      (hit-deltas a t target (crit? a t)))))

(def ^:private ^:const fire-seconds 8)
(def ^:private ^:const lava-seconds 15)
(def ^:private ^:const lava-damage 4.0)
(defn- box-of
  "Returns the width and height of an entity."
  [e]
  (if (= :player (:type e))
    [player-half player-height]
    (let [{:keys [half height]} (mobs/types (:type e))] [half height])))

(defn- near-edits?
  "Returns true when an entity stands in a part of the world that is being kept
   up to date."
  [world e]
  (let [chunks (:chunks world)]
    (when (seq chunks)
      (let [p (:pos e)
            cx (fn ^long [^double a] (bit-shift-right (long (Math/floor a)) 4))
            x0 (cx (- (v/x p) 0.5)) x1 (cx (+ (v/x p) 0.5))
            z0 (cx (- (v/z p) 0.5)) z1 (cx (+ (v/z p) 0.5))]
        (or (some? (get chunks (chunk/pos->id x0 z0)))
            (and (not= x0 x1) (some? (get chunks (chunk/pos->id x1 z0))))
            (and (not= z0 z1) (some? (get chunks (chunk/pos->id x0 z1))))
            (and (not= x0 x1) (not= z0 z1)
                 (some? (get chunks (chunk/pos->id x1 z1)))))))))

(def ^:private ^:const sunk-shrink-xz 0.1)
(def ^:private ^:const sunk-shrink-y 0.4)
(def ^:private ^:const touch-bit 1)
(def ^:private ^:const sunk-bit 2)

(defn- floor-lo ^long [^double a] (long (Math/floor (+ a fluid-margin))))
(defn- floor-hi ^long [^double a] (long (Math/floor (+ (- a fluid-margin) 1.0))))

(defn- span
  "Returns the blocks an entity's body reaches into, shrunk by the given
   margins."
  [p ^double half ^double height [sxz sy]]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        sy (double sy)
        h (- half (double sxz))]
    [(floor-lo (- px h)) (floor-hi (+ px h))
     (max chunk/min-y (floor-lo (+ py sy)))
     (min (inc chunk/max-y) (floor-hi (- (+ py height) sy)))
     (floor-lo (- pz h)) (floor-hi (+ pz h))]))

(defn- mark-cell
  "Notes in the running flags whether a block touches or engulfs an entity in
   fire."
  [chunks ^longs acc x y z inner?]
  (let [st (chunk/block-state chunks (gen/flat-chunk) x y z)
        lava? (= :lava (liquid/liquid-class st))]
    (when (or lava? (block/fire? st))
      (aset acc 0 (bit-or (aget acc 0) touch-bit)))
    (when (and lava? inner?)
      (aset acc 0 (bit-or (aget acc 0) sunk-bit)))))

(defn- scan-z
  "Walks a row of blocks, noting the fire an entity touches."
  [chunks ^longs acc outer inner x y yin?]
  (let [z1 (outer 5) iz0 (inner 4) iz1 (inner 5)]
    (loop [z (outer 4)]
      (when (and (< z z1) (not= 3 (aget acc 0)))
        (mark-cell chunks acc x y z (and yin? (>= z iz0) (< z iz1)))
        (recur (inc z))))))

(defn- scan-y
  "Walks a column of blocks, noting the fire an entity touches."
  [chunks ^longs acc outer inner x xin?]
  (let [y1 (outer 3) iy0 (inner 2) iy1 (inner 3)]
    (loop [y (outer 2)]
      (when (and (< y y1) (not= 3 (aget acc 0)))
        (scan-z chunks acc outer inner x y (and xin? (>= y iy0) (< y iy1)))
        (recur (inc y))))))

(defn- probe
  "Returns flags saying whether an entity touches fire and whether it is sunk in
   lava."
  ^long [world e]
  (let [[half height] (box-of e)
        p (:pos e)
        outer (span p (double half) (double height) [fluid-margin fluid-margin])
        inner (span p (double half) (double height) [sunk-shrink-xz sunk-shrink-y])
        chunks (:chunks world)
        x1 (outer 1) ix0 (inner 0) ix1 (inner 1)
        ^longs acc (long-array 1)]
    (loop [x (outer 0)]
      (when (and (< x x1) (not= 3 (aget acc 0)))
        (scan-y chunks acc outer inner x (and (>= x ix0) (< x ix1)))
        (recur (inc x))))
    (aget acc 0)))

(defn- burning-flag
  "Returns the deltas that start or stop showing an entity as alight."
  [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(defn- burn-tick-deltas
  "Returns the deltas for an entity burning down another tick."
  [eid ^long fire wet?]
  (when (and (pos? fire) (not wet?))
    (cond-> [[:merge-entity eid {:fire (dec fire)}]]
            (zero? (rem fire fire-damage-period)) (conj [:damage eid 1.0]))))

(defn- ignite-deltas
  "Returns the deltas that hurt an entity and set it alight."
  [eid fire wet? damage seconds]
  (cond-> [[:damage eid (double damage)]]
          (not wet?) (conj [:merge-entity eid {:fire (max (long fire) (* ticks-per-second (long seconds)))}])))

(defn- douse-deltas
  "Returns the deltas that put an entity's fire out."
  [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz
                      (mapv (fn [c] (long (Math/floor (double c))))
                            [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))])))])))

(defn- fire-deltas
  "Returns the deltas for one entity and fire this tick."
  [world eid e]
  (let [fire (long (or (:fire e) 0))
        wet? (boolean (:wet? e))
        flags (probe world e)
        touch (not (zero? (bit-and flags touch-bit)))
        sunk? (not (zero? (bit-and flags sunk-bit)))
        flag (burning-flag eid e fire sunk?)]
    (if (creative-proof? e)
      (concat flag (when (pos? fire) [[:merge-entity eid {:fire 0}]]))
      (concat flag
              (burn-tick-deltas eid fire wet?)
              (when touch (ignite-deltas eid fire wet? 1.0 fire-seconds))
              (when sunk? (ignite-deltas eid fire wet? lava-damage lava-seconds))
              (douse-deltas eid e fire wet?)))))

(def ^:private ^:const safe-fall 3.0)
(defn- landing-particles
  "Returns the deltas for the particles of a hard landing."
  [world e ^double fall]
  (let [power (Math/floor (+ (- fall safe-fall) 1.0e-6))
        pos (:pos e)
        st (chunk/chunks-get-block (:chunks world) (gen/flat-chunk)
                                   [(long (Math/floor (v/x pos))) (long (Math/floor (- (v/y pos) 0.2)))
                                    (long (Math/floor (v/z pos)))])]
    (when (and (pos? power) (not (block/air? st)))
      (let [scale (min (+ 0.2 (/ power 15.0)) 2.5)]
        [(out/all (out/particles :block st [(v/x pos) (v/y pos) (v/z pos)] (long (* 150.0 scale)) 0.15))]))))

(defn- landing-deltas
  "Returns the deltas for an entity that has just hit the ground."
  [world eid e]
  (when-let [fall (:landed e)]
    (concat [[:merge-entity eid {:landed nil}]]
            (landing-particles world e (double fall)))))

(defn- void-deltas
  "Returns the deltas that hurt an entity fallen out of the world."
  [eid e]
  (when (and (pos? (double (:health e))) (< (v/y (:pos e)) void-y))
    [[:damage eid void-damage]]))

(defn- report-deltas
  "Returns the deltas that show an entity taking damage."
  [world eid e]
  (let [health (double (:health e))
        shown (double (or (:health-sent e) health))]
    (when (< health shown)
      (concat
        [[:merge-entity eid (cond-> {:health-sent health}
                                    (not= :player (:type e))
                                    (assoc :panic-until (+ (long (:tick world)) panic-ticks)))]]
        (when-let [snd (hurt-sound e)]
          [(out/all (out/sound snd (:pos e) 1.0 (sound-pitch world eid e)))])
        [(out/all (out/status eid (if (pos? health) :hurt :death)))]
        (when (= :player (:type e))
          [(out/to eid (out/health health))])))))

(defn- timer-deltas
  "Returns the deltas that run down an entity's hurt and death timers."
  [eid e]
  (let [resist (long (or (:hurt-resist e) 0))
        dead? (not (pos? (double (:health e))))
        death (when dead? (inc (long (or (:death-time e) 0))))]
    (concat
      (when (or (pos? resist) death)
        [[:merge-entity eid (cond-> {}
                                    (pos? resist) (assoc :hurt-resist (dec resist))
                                    death (assoc :death-time death))]])
      (when (and death (>= (long death) death-ticks) (not= :player (:type e)))
        [[:remove-entity eid]]))))

(defn- respawn-config
  "Returns where a player asked to come back, if anywhere."
  [e]
  (if-let [{:keys [pos yaw pitch]} (:forced-spawn e)]
    {:pos pos :yaw (double (or yaw 0.0)) :pitch (double (or pitch 0.0)) :forced? true}
    (when-let [pos (:spawn e)]
      {:pos pos :yaw (:yaw e 0.0) :pitch 0.0 :forced? false})))

(defn- free-to-stand?
  "Returns true when a player can stand at a position."
  [chunks [x y z]]
  (and (block/possible-to-respawn-in? (chunk/chunks-get-block chunks (gen/flat-chunk) [x y z]))
       (block/possible-to-respawn-in? (chunk/chunks-get-block chunks (gen/flat-chunk) [x (inc (long y)) z]))))

(defn- found-respawn
  "Returns the place and direction a player actually comes back at, or nothing
   when their spawn is gone."
  [chunks {:keys [pos yaw pitch forced?]}]
  (if (bed/head-pos chunks pos)
    (let [up (bed/stand-up-position chunks pos yaw)]
      [up (bed/look-yaw pos up) 0.0])
    (when (and forced? (free-to-stand? chunks pos))
      [[(+ (double (nth pos 0)) 0.5) (+ (double (nth pos 1)) 0.1) (+ (double (nth pos 2)) 0.5)]
       yaw pitch])))

(defn- respawn-point
  "Returns where a player comes back, and the message to show if their own spawn
   was lost."
  [world eid e]
  (let [cfg (respawn-config e)
        chunks (:chunks world)]
    (if-let [[pos yaw pitch] (and cfg (found-respawn chunks cfg))]
      [pos yaw pitch nil]
      [(state/world-spawn-pos world eid) 0.0 0.0
       (when cfg (out/overlay [{:translate "block.minecraft.spawn.not_valid"}]))])))

(defn- reshow-deltas
  "Returns the deltas that show a respawned player to everyone watching them
   again."
  [world eid]
  (for [[oid o] (:entities world)
        :when (contains? (:tracking o) eid)]
    [:tracking oid [] [eid]]))

(defn- revived
  "Returns the deltas that put a player back in the world whole."
  [eid e pos yaw pitch]
  [[:teleport eid pos]
   [:merge-entity eid {:health      player-health
                       :health-sent player-health
                       :hurt-resist 0 :last-damage 0.0 :death-time 0}]
   (out/to eid (out/respawn))
   (out/to eid (out/teleport pos yaw pitch))
   (out/to eid (out/health player-health))
   (out/to eid (out/held-slot (long (or (:held-slot e) 0))))])

(defn- respawn-deltas
  "Returns the deltas for a dead player asking to come back."
  [world eid]
  (let [e (get-in world [:entities eid])]
    (when (and e (not (pos? (double (:health e)))))
      (let [[pos yaw pitch lost] (respawn-point world eid e)
            inv (:inventory e)]
        (cond-> (revived eid e pos yaw pitch)
                (seq inv) (conj (out/to eid (out/inventory (mapv inv (range menu/slot-count)) (:carried e))))
                lost (conj (out/to eid lost))
                true (into (reshow-deltas world eid)))))))

(defn- idle?
  "Returns true when nothing is happening to an entity."
  [e]
  (let [health (double (or (:health e) 0.0))]
    (and (pos? health)
         (>= (v/y (:pos e)) void-y)
         (zero? (long (or (:fire e) 0)))
         (not (:burning? e))
         (zero? (long (or (:hurt-resist e) 0)))
         (nil? (:landed e))
         (>= health (double (or (:health-sent e) health))))))

(defn- living-deltas
  "Returns the deltas for one living entity this tick."
  [world eid e]
  (let [busy? (not (idle? e))]
    (concat (when busy? (timer-deltas eid e))
            (when busy? (void-deltas eid e))
            (when busy? (landing-deltas world eid e))
            (fire-deltas world eid e)
            (when busy? (report-deltas world eid e)))))

(defn- live-entries
  "Returns the entities that need a look this tick."
  [world]
  (into []
        (filter (fn [[_ e]] (and (some? (:health e))
                                 (or (not (idle? e)) (near-edits? world e)))))
        (:entities world)))

(defn- living-batch-fn [world batch]
  #(into [] (mapcat (fn [[eid e]] (living-deltas world eid e))) batch))

(defn- living-fns [world]
  (mapv (fn [batch] (living-batch-fn world batch))
        (partition-all 32 (live-entries world))))

(defn- event-deltas
  "Returns the deltas for one tick's attacks and respawn requests."
  [world events]
  (into []
        (mapcat (fn [[tag eid :as ev]]
                  (case tag
                    :attack (attack-deltas world ev)
                    :respawn (respawn-deltas world eid)
                    nil)))
        events))

(defn damage
  "Returns the deltas for what hurts, kills and revives entities this tick."
  [world d]
  (let [events (:input d)]
    (conj (living-fns world) #(event-deltas world events))))
