(ns collider.game.systems.damage
  "Damage from attacks, fire, the void and falls, with death and respawn."
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
            [collider.game.systems.containers :as containers]
            [collider.game.out :as out]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(def ^:private ^:const void-y (- chunk/min-y 64.0))
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
      (mobs/hurt-sound (:type e))
      (mobs/death-sound (:type e)))))

(defn- sound-pitch ^double [world eid e]
  (let [t (long (:tick world))
        base (if (:baby-until e) baby-voice-pitch adult-voice-pitch)
        r (- (random/of-longs t eid (hash :hurt1)) (random/of-longs t eid (hash :hurt2)))]
    (+ base (* voice-pitch-spread r))))

(defn- creative-proof?
  "Returns true when nothing can hurt the entity. Players are always in creative
   mode."
  [e]
  (= :player (:type e)))

(defn- held-item [e]
  (:item (get (:inventory e) (+ 36 (long (or (:held-slot e) 0))))))

(defn- dist-sq ^double [a b]
  (let [dx (- (v/x a) (v/x b)) dy (- (v/y a) (v/y b)) dz (- (v/z a) (v/z b))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- eye-of [e]
  (let [p (:pos e)]
    [(v/x p) (+ (v/y p) (entity/eye-height e)) (v/z p)]))

(defn- ray-steps ^long [[ax ay az] [bx by bz]]
  (let [dx (- (double bx) (double ax))
        dy (- (double by) (double ay))
        dz (- (double bz) (double az))]
    (long (Math/ceil (/ (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) 0.1)))))

(defn- ray-cell [[ax ay az] [bx by bz] ^double s]
  [(long (Math/floor (+ (double ax) (* (- (double bx) (double ax)) s))))
   (long (Math/floor (+ (double ay) (* (- (double by) (double ay)) s))))
   (long (Math/floor (+ (double az) (* (- (double bz) (double az)) s))))])

(defn- clear-ray? [chunks a b ^long n]
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

(defn- sees? [world a b]
  (let [pa (eye-of a) pb (eye-of b)]
    (clear-ray? (:chunks world) pa pb (ray-steps pa pb))))

(defn- attackable? [a t]
  (and a t (:health t) (pos? (double (:health t))) (not (creative-proof? t))))

(defn- in-reach? [world a t]
  (let [d2 (dist-sq (:pos a) (:pos t))]
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
  (let [yaw (Math/toRadians (double (:yaw a)))]
    [:push target [(* (- (Math/sin yaw)) knockback-attack-strength) knockback-lift
                   (* (Math/cos yaw) knockback-attack-strength)]]))

(defn- hit-deltas [a t target crit?]
  (cond-> [[:damage target (melee-damage a crit?)
            (- (v/x (:pos a)) (v/x (:pos t)))
            (- (v/z (:pos a)) (v/z (:pos t)))]
           [:merge-entity target {:love-until nil}]]
          (:sprinting? a) (conj (sprint-push a target))
          crit? (into [(out/all (out/animation target :crit))])))

(defn- attack-deltas [world [_ eid target]]
  (let [a (get-in world [:entities eid])
        t (get-in world [:entities target])]
    (when (and (attackable? a t) (in-reach? world a t))
      (hit-deltas a t target (crit? a t)))))

(def ^:private ^:const fire-seconds 8)
(def ^:private ^:const lava-seconds 15)
(def ^:private ^:const lava-damage 4.0)
(defn- box-of [e]
  (if (= :player (:type e))
    [player-half player-height]
    (let [{:keys [half height]} (mobs/types (:type e))] [half height])))

(defn- near-edits?
  "Returns true when the entity stands in a chunk the world holds."
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
  "Returns the blocks the body of an entity reaches into, shrunk by the given
   margins."
  [p ^double half ^double height [sxz sy]]
  (let [px (v/x p) py (v/y p) pz (v/z p)
        sy (double sy)
        h (- half (double sxz))]
    [(floor-lo (- px h)) (floor-hi (+ px h))
     (max chunk/min-y (floor-lo (+ py sy)))
     (min (inc chunk/max-y) (floor-hi (- (+ py height) sy)))
     (floor-lo (- pz h)) (floor-hi (+ pz h))]))

(defn- mark-cell [chunks ^longs acc x y z inner?]
  (let [st (chunk/block-state chunks (gen/flat-chunk) x y z)
        lava? (= :lava (liquid/liquid-class st))]
    (when (or lava? (block/fire? st))
      (aset acc 0 (bit-or (aget acc 0) touch-bit)))
    (when (and lava? inner?)
      (aset acc 0 (bit-or (aget acc 0) sunk-bit)))))

(defn- scan-z [chunks ^longs acc outer inner x y yin?]
  (let [z1 (outer 5) iz0 (inner 4) iz1 (inner 5)]
    (loop [z (outer 4)]
      (when (and (< z z1) (not= 3 (aget acc 0)))
        (mark-cell chunks acc x y z (and yin? (>= z iz0) (< z iz1)))
        (recur (inc z))))))

(defn- scan-y [chunks ^longs acc outer inner x xin?]
  (let [y1 (outer 3) iy0 (inner 2) iy1 (inner 3)]
    (loop [y (outer 2)]
      (when (and (< y y1) (not= 3 (aget acc 0)))
        (scan-z chunks acc outer inner x y (and xin? (>= y iy0) (< y iy1)))
        (recur (inc y))))))

(defn- probe
  "Returns the touch and sunk bits for an entity in fire or lava."
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

(defn- burning-flag [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(defn- burn-tick-deltas [eid ^long fire wet?]
  (when (and (pos? fire) (not wet?))
    (cond-> [[:merge-entity eid {:fire (dec fire)}]]
            (zero? (rem fire fire-damage-period)) (conj [:damage eid 1.0]))))

(defn- ignite-deltas [eid fire wet? damage seconds]
  (cond-> [[:damage eid (double damage)]]
          (not wet?) (conj [:merge-entity eid {:fire (max (long fire) (* ticks-per-second (long seconds)))}])))

(defn- douse-deltas [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz
                      (mapv (fn [c] (long (Math/floor (double c))))
                            [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))])))])))

(defn- fire-deltas [world eid e]
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
(defn- landing-particles [world e ^double fall]
  (let [power (Math/floor (+ (- fall safe-fall) 1.0e-6))
        pos (:pos e)
        st (chunk/chunks-get-block (:chunks world) (gen/flat-chunk)
                                   [(long (Math/floor (v/x pos))) (long (Math/floor (- (v/y pos) 0.2)))
                                    (long (Math/floor (v/z pos)))])]
    (when (and (pos? power) (not (block/air? st)))
      (let [scale (min (+ 0.2 (/ power 15.0)) 2.5)]
        [(out/all (out/particles :block st [(v/x pos) (v/y pos) (v/z pos)] (long (* 150.0 scale)) 0.15))]))))

(defn- landing-deltas [world eid e]
  (when-let [fall (:landed e)]
    (concat [[:merge-entity eid {:landed nil}]]
            (landing-particles world e (double fall)))))

(defn- void-deltas [eid e]
  (when (and (pos? (double (:health e))) (< (v/y (:pos e)) void-y))
    [[:damage eid void-damage]]))

(defn- panicked [world e]
  (cond-> {:love-until nil :no-action 0}
          (>= (v/y (:pos e)) void-y) (assoc :panic-until (+ (long (:tick world)) panic-ticks))))

(defn- report-deltas [world eid e]
  (let [health (double (:health e))
        shown (double (or (:health-sent e) health))]
    (when (< health shown)
      (concat
        [[:merge-entity eid (cond-> {:health-sent health}
                                    (not= :player (:type e))
                                    (merge (panicked world e)))]]
        (when-let [snd (hurt-sound e)]
          [(out/all (out/sound snd (:pos e) 1.0 (sound-pitch world eid e)))])
        [(out/all (out/status eid (if (pos? health) :hurt :death)))]
        (when (= :player (:type e))
          [(out/to eid (out/health health))])))))

(defn- timer-deltas [eid e]
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

(defn- respawn-config [e]
  (if-let [{:keys [pos yaw pitch]} (:forced-spawn e)]
    {:pos pos :yaw (double (or yaw 0.0)) :pitch (double (or pitch 0.0)) :forced? true}
    (when-let [pos (:spawn e)]
      {:pos pos :yaw (:yaw e 0.0) :pitch 0.0 :forced? false})))

(defn- free-to-stand? [chunks [x y z]]
  (and (block/possible-to-respawn-in? (chunk/chunks-get-block chunks (gen/flat-chunk) [x y z]))
       (block/possible-to-respawn-in? (chunk/chunks-get-block chunks (gen/flat-chunk) [x (inc (long y)) z]))))

(defn- found-respawn [chunks {:keys [pos yaw pitch forced?]}]
  (if (bed/head-pos chunks pos)
    (let [up (bed/stand-up-position chunks pos yaw)]
      [up (bed/look-yaw pos up) 0.0])
    (when (and forced? (free-to-stand? chunks pos))
      [[(+ (double (nth pos 0)) 0.5) (+ (double (nth pos 1)) 0.1) (+ (double (nth pos 2)) 0.5)]
       yaw pitch])))

(defn- respawn-point [world eid e]
  (let [cfg (respawn-config e)
        chunks (:chunks world)]
    (if-let [[pos yaw pitch] (and cfg (found-respawn chunks cfg))]
      [pos yaw pitch nil]
      [(state/world-spawn-pos world eid) 0.0 0.0
       (when cfg (out/overlay [{:translate "block.minecraft.spawn.not_valid"}]))])))

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

(defn- respawn-deltas [world eid]
  (let [e (get-in world [:entities eid])]
    (when (and e (not (pos? (double (:health e)))))
      (let [[pos yaw pitch lost] (respawn-point world eid e)
            inv (apply dissoc (:inventory e) (range 5))]
        (cond-> (into (vec (containers/removed-deltas world eid e))
                      (revived eid e pos yaw pitch))
                (seq inv) (conj (out/to eid (own-slots inv)))
                lost (conj (out/to eid lost))
                true (into (reshow-deltas world eid)))))))

(defn- idle? [e]
  (let [health (double (or (:health e) 0.0))]
    (and (pos? health)
         (>= (v/y (:pos e)) void-y)
         (zero? (long (or (:fire e) 0)))
         (not (:burning? e))
         (zero? (long (or (:hurt-resist e) 0)))
         (nil? (:landed e))
         (>= health (double (or (:health-sent e) health))))))

(defn- living-deltas [world eid e]
  (let [busy? (not (idle? e))]
    (concat (when busy? (timer-deltas eid e))
            (when busy? (void-deltas eid e))
            (when busy? (landing-deltas world eid e))
            (fire-deltas world eid e)
            (when busy? (report-deltas world eid e)))))

(defn- live-entries [world]
  (into []
        (filter (fn [[_ e]] (and (some? (:health e)) (not= :item (:type e))
                                 (or (not (idle? e)) (near-edits? world e)))))
        (:entities world)))

(defn- living-batch-fn [world batch]
  #(into [] (mapcat (fn [[eid e]] (living-deltas world eid e))) batch))

(defn- living-fns [world]
  (mapv (fn [batch] (living-batch-fn world batch))
        (partition-all 32 (live-entries world))))

(defn- event-deltas [world events]
  (into []
        (mapcat (fn [[tag eid :as ev]]
                  (case tag
                    :attack (attack-deltas world ev)
                    :respawn (respawn-deltas world eid)
                    nil)))
        events))

(defn damage [world d]
  (let [events (:input d)]
    (conj (living-fns world) #(event-deltas world events))))
