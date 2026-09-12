(ns collider.game.systems.damage
  (:require [collider.game.entity :as entity]
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
(def ^:private material-bonus
  {"wooden" 0.0 "golden" 0.0 "stone" 1.0 "iron" 2.0 "diamond" 3.0 "netherite" 4.0})
(def ^:private tool-base
  {"sword" 4.0 "axe" 3.0 "pickaxe" 2.0 "shovel" 1.0})

(defn- weapon-damage ^double [item]
  (if-let [[_ m t] (when (keyword? item) (re-matches #"(\w+)-(\w+)" (name item)))]
    (if-let [base (tool-base t)]
      (+ (double base) (double (get material-bonus m 0.0)))
      0.0)
    0.0))

(defn- hurt-sound [e]
  (if (= :player (:type e))
    (cond
      (not (pos? (double (:health e)))) :player/death
      (pos? (long (or (:fire e) 0))) :player/hurt-on-fire
      :else :player/hurt)
    (mobs/say-sound (:type e))))

(defn- sound-pitch ^double [world eid e]
  (let [t (long (:tick world))
        base (if (:baby-until e) 1.5 1.0)
        r (- (random/of-longs t eid (hash :hurt1)) (random/of-longs t eid (hash :hurt2)))]
    (+ base (* 0.2 r))))

(defn- creative-proof? [e]
  (= :player (:type e)))

(defn- held-item [e]
  (:item (get (:inventory e) (+ 36 (long (or (:held-slot e) 0))))))

(defn- dist-sq ^double [a b]
  (let [dx (- (v/x a) (v/x b)) dy (- (v/y a) (v/y b)) dz (- (v/z a) (v/z b))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- sees? [world a b]
  (let [pa (:pos a) pb (:pos b)
        ax (v/x pa) ay (+ (v/y pa) (entity/eye-height a)) az (v/z pa)
        bx (v/x pb) by (+ (v/y pb) (entity/eye-height b)) bz (v/z pb)
        dx (- bx ax) dy (- by ay) dz (- bz az)
        len (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
        n (long (Math/ceil (/ len 0.1)))
        chunks (:chunks world)]
    (loop [i 1]
      (if (>= i n)
        true
        (let [s (/ (double i) n)]
          (if (phys/solid? chunks gen/flat-chunk
                           (long (Math/floor (+ ax (* dx s))))
                           (long (Math/floor (+ ay (* dy s))))
                           (long (Math/floor (+ az (* dz s)))))
            false
            (recur (inc i))))))))

(defn- attackable? [a t]
  (and a t (:health t) (pos? (double (:health t))) (not (creative-proof? t))))

(defn- in-reach? [world a t]
  (< (dist-sq (:pos a) (:pos t))
     (if (sees? world a t) reach-sq blind-reach-sq)))

(defn- crit? [a t]
  (and (not (:on-ground a))
       (neg? (v/y (or (:client-vel a) [0.0 0.0 0.0])))
       (not (:wet? t))))

(defn- melee-damage ^double [a crit?]
  (cond-> (+ base-damage (weapon-damage (held-item a)))
          crit? (* 1.5)))

(defn- sprint-push [a target]
  (let [yaw (Math/toRadians (double (:yaw a)))]
    [:push target [(* (- (Math/sin yaw)) 0.5) 0.1 (* (Math/cos yaw) 0.5)]]))

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
    [0.3 1.8]
    (let [{:keys [half height]} (mobs/types (:type e))] [(or half 0.45) (or height 1.3)])))

(defn- near-edits? [world e]
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

(defn- probe [world e shrink-xz shrink-y lava-only?]
  (let [[half height] (box-of e)
        p (:pos e)
        shrink-xz (double shrink-xz)
        shrink-y (double shrink-y)
        half (- (double half) shrink-xz)
        chunks (:chunks world)
        fl (fn ^long [^double a] (long (Math/floor (+ a 0.001))))
        ce (fn ^long [^double a] (long (Math/floor (+ (- a 0.001) 1.0))))
        x0 (fl (- (v/x p) half)) x1 (ce (+ (v/x p) half))
        y0 (max chunk/min-y (fl (+ (v/y p) shrink-y)))
        y1 (min (inc chunk/max-y) (ce (- (+ (v/y p) (double height)) shrink-y)))
        z0 (fl (- (v/z p) half)) z1 (ce (+ (v/z p) half))]
    (loop [x x0]
      (when (< x x1)
        (or (loop [y y0]
              (when (< y y1)
                (or (loop [z z0]
                      (when (< z z1)
                        (let [st (chunk/block-state chunks gen/flat-chunk x y z)]
                          (cond (and (block/fire? st) (not lava-only?)) :fire
                                (= :lava (liquid/liquid-class st)) :lava
                                :else (recur (inc z))))))
                    (recur (inc y)))))
            (recur (inc x)))))))

(defn- burning-flag [eid e ^long fire sunk?]
  (let [lit? (boolean (or (pos? fire) sunk?))]
    (when (not= lit? (boolean (:burning? e)))
      [[:merge-entity eid {:burning? lit?}]])))

(defn- burn-tick-deltas [eid ^long fire wet?]
  (when (and (pos? fire) (not wet?))
    (cond-> [[:merge-entity eid {:fire (dec fire)}]]
            (zero? (rem fire 20)) (conj [:damage eid 1.0]))))

(defn- ignite-deltas [eid fire wet? damage seconds]
  (cond-> [[:damage eid (double damage)]]
          (not wet?) (conj [:merge-entity eid {:fire (max (long fire) (* 20 (long seconds)))}])))

(defn- douse-deltas [eid e fire wet?]
  (when (and wet? (pos? (long fire)))
    (cons [:merge-entity eid {:fire 0 :burning? false}]
          [(out/all (out/fizz
                      (mapv (fn [c] (long (Math/floor (double c))))
                            [(v/x (:pos e)) (v/y (:pos e)) (v/z (:pos e))])))])))

(defn- fire-deltas [world eid e]
  (let [fire (long (or (:fire e) 0))
        wet? (boolean (:wet? e))
        touch (probe world e 0.001 0.001 false)
        sunk? (some? (probe world e 0.1 0.4 true))
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
        st (chunk/chunks-get-block (:chunks world) gen/flat-chunk
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

(defn- report-deltas [world eid e]
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
  (and (block/possible-to-respawn-in? (chunk/chunks-get-block chunks gen/flat-chunk [x y z]))
       (block/possible-to-respawn-in? (chunk/chunks-get-block chunks gen/flat-chunk [x (inc (long y)) z]))))

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

(defn- respawn-deltas [world eid]
  (let [e (get-in world [:entities eid])]
    (when (and e (not (pos? (double (:health e)))))
      (let [[pos yaw pitch lost] (respawn-point world eid e)
            inv (:inventory e)]
        (cond-> [[:teleport eid pos]
                 [:merge-entity eid {:health      player-health
                                     :health-sent player-health
                                     :hurt-resist 0 :last-damage 0.0 :death-time 0}]
                 (out/to eid (out/respawn))
                 (out/to eid (out/teleport pos yaw pitch))
                 (out/to eid (out/health player-health))
                 (out/to eid (out/held-slot (long (or (:held-slot e) 0))))]
                (seq inv) (conj (out/to eid (out/inventory (mapv inv (range menu/slot-count)) (:carried e))))
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

(defn- living-fns [world]
  (let [alive (into []
                    (filter (fn [[_ e]] (and (some? (:health e))
                                             (or (not (idle? e)) (near-edits? world e)))))
                    (:entities world))]
    (mapv (fn [batch]
            #(into []
                   (mapcat (fn [[eid e]]
                             (let [busy? (not (idle? e))]
                               (concat (when busy? (timer-deltas eid e))
                                       (when busy? (void-deltas eid e))
                                       (when busy? (landing-deltas world eid e))
                                       (fire-deltas world eid e)
                                       (when busy? (report-deltas world eid e))))))
                   batch))
          (partition-all 32 alive))))

(defn- event-deltas [world events]
  (into []
        (mapcat (fn [[tag eid :as ev]]
                  (case tag
                    :attack (attack-deltas world ev)
                    :respawn (respawn-deltas world eid)
                    nil)))
        events))

(defn damage [world events]
  (conj (living-fns world) #(event-deltas world events)))
