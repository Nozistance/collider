(ns collider.game.systems.mobs
  "Mob thinking, movement and sounds."
  (:require [collider.random :as random]
            [collider.game.entity :as entity]
            [collider.vec :as v]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.control :as control]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.sheep :as sheep]
            [collider.game.mob.cow :as cow]
            [collider.game.mob.mooshroom :as mooshroom]
            [collider.game.mob.push :as push]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.sense :as sense]
            [collider.game.state :as state]
            [collider.game.out :as out]
            [collider.world.block :as block]
            [collider.world.blocks.liquid :as liquid]
            [collider.game.systems.blocks.reach :as reach]
            [collider.world.env.signal :as signal]
            [collider.world.phys :as phys])
  (:import (collider.game.entity Mob)
           (collider.world.phys Move)))

(set! *warn-on-reflection* true)

(def ^:private brains {:sheep sheep/brain :cow cow/brain :mooshroom mooshroom/brain})

(def ^:private specs {:sheep sheep/spec :cow cow/spec :mooshroom mooshroom/spec})

(defn- think [world eid e t tempters]
  (if-let [b (brains (:type e))]
    (b world eid e t tempters)
    [e nil]))

(def ^:private ^:const reach-buffer 3.0)

(defn- in-reach?
  "Tells whether the mob's box is within the player's entity reach.
  Vanilla checks the same box from the eye, buffer and all."
  [p e]
  (let [[half height] (mobs/box-of e)
        [ex ey ez] (reach/eye-pos p)
        [x y z] (:pos e)
        w (* 2.0 (double half))
        dx (reach/axis-gap ex (- (double x) (double half)) w)
        dy (reach/axis-gap ey (double y) (double height))
        dz (reach/axis-gap ez (- (double z) (double half)) w)
        r (+ (state/entity-reach p) reach-buffer)]
    (< (+ (* dx dx) (* dy dy) (* dz dz)) (* r r))))

(defn- ctx-of
  "Returns what an interact event gives its handlers, nil when the
  click reaches nothing: no mob, or one too far away."
  [world t [_ peid target hand sneaking?]]
  (let [p (get-in world [:entities peid])
        e (get-in world [:entities target])
        hand (if (#{:off 1} hand) :off :main)]
    (when (and p e (mobs/mob-type? (:type e)))
      (let [p (assoc p :sneaking? (boolean sneaking?))]
        (when (in-reach? p e)
          {:world world :t t :peid peid :p p :eid target :e e
           :hand hand :item (sense/in-hand p hand)})))))

(defn- name-tag-result
  "The place of the name tag in the chain; naming waits for its own
  ticket and lets every other handler have the click."
  [_ctx]
  nil)

(defn- leash-result
  "The place of Entity.interact in the chain; the lead waits for its
  own ticket."
  [_ctx]
  nil)

(defn- equippable-result
  "The place of wearable items in the chain; saddles and armour wait
  for their own ticket."
  [_ctx]
  nil)

(defn- species-result [ctx]
  (let [fs (case (:type (:e ctx))
             :sheep [sheep/shear-result animal/feed-result]
             :cow [cow/milk-result animal/feed-result]
             :mooshroom [mooshroom/bowl-result mooshroom/shear-result
                         mooshroom/flower-result cow/milk-result
                         animal/feed-result]
             nil)]
    (some (fn [f] (f ctx)) fs)))

(def ^:private chain
  [name-tag-result (partial animal/egg-result specs) leash-result
   species-result sheep/dye-result equippable-result])

(defn- answered
  "Returns the deltas the whole chain leaves behind. A result that
  takes the click is heard as a game event, and the server swings
  the arm for the player when the client will not."
  [{:keys [peid p e hand t]} {:keys [result deltas]}]
  (cond-> (vec deltas)
    (not= :pass result)
    (into (signal/game-event :entity-interact (:pos e) peid))
    (= :success-server result)
    (into (state/swing-deltas peid p hand t true))))

(defn- interact
  "Returns the deltas of one player click on a mob. The handlers run
  in the vanilla order and the first one that does not pass has the
  click; one out of reach is dropped without a word."
  [world ev t]
  (if-let [ctx (ctx-of world t ev)]
    (answered ctx (or (some (fn [f] (f ctx)) chain) {:result :pass}))
    []))

(defn- interact-deltas [world events t]
  (state/fold-events world (filter #(= :interact (first %)) events)
                     (fn [w ev] (interact w ev t))))

(def ^:private zero3 (v/v3 0.0 0.0 0.0))

(def ^:private ^:const gravity 0.08)

(def ^:private ^:const jump-speed 0.42)

(def ^:private ^:const ground-friction 0.546)

(def ^:private ^:const air-friction 0.91)

(def ^:private ^:const water-friction 0.8)

(def ^:private ^:const air-accel 0.02)

(defn- in-liquid?
  "Whether the box of a mob of that height meets the liquid kind.
  As updateFluidHeightAndDoFluidPushing: any height above zero in
  the box less its 0.4 margins counts."
  [world p ^double height kind?]
  (let [y (v/y p)
        cx (long (Math/floor (v/x p)))
        cz (long (Math/floor (v/z p)))
        y1 (long (Math/floor (- (+ y (double height)) 0.4)))]
    (loop [cy (long (Math/floor (+ y 0.4)))]
      (cond
        (> cy y1) false
        (kind? (sense/block-at world cx cy cz)) true
        :else (recur (inc cy))))))

(defn- in-water? [world p height]
  (in-liquid? world p (double height) block/water?))

(defn- in-lava? [world p height]
  (in-liquid? world p (double height) block/lava?))

(defn- water-above? [world p]
  (let [x (long (Math/floor (v/x p)))
        y (long (Math/floor (+ (v/y p) 0.6)))
        z (long (Math/floor (v/z p)))]
    (block/water? (sense/block-at world x y z))))

(defn- heading
  "Returns the unit way the mob faces, the way travel drives it."
  [e]
  (let [r (Math/toRadians (double (:yaw e)))]
    [(- (Math/sin r)) (Math/cos r)]))

(defn- look-toward [e height o]
  (let [[_ y _] (:pos e)
        [_ oy _] (:pos o)
        eye (+ (double y) (* 0.95 (double height)))
        oeye (+ (double oy)
                (if (= :player (:type o))
                  1.62
                  (* 0.95 (double (get-in mobs/types [(:type o) :height] 1.0)))))
        dh (Math/sqrt (v/dist-sq (:pos e) (:pos o)))]
    [(v/yaw-toward (:pos e) (:pos o))
     (- (Math/toDegrees (Math/atan2 (- oeye eye) dh)))]))

(defn- active-look [e ^long t]
  (let [look (:look e)]
    (when (and look (> (long (:until look 0)) t)) look)))

(defn- look-angles [world e height look]
  (let [target (when-let [oid (:target look)] (get-in world [:entities oid]))]
    (cond
      target (look-toward e height target)
      (and look (:yaw look)) [(:yaw look) 0.0]
      :else [(:yaw e) 0.0])))

(defn- head-same? [e look ^double hy ^double hp]
  (and (identical? look (:look e))
       (let [oh (:head-yaw e)] (and oh (== (double oh) hy)))
       (let [op (:pitch e)] (and op (== (double op) hp)))))

(defn- head-update [world e height t moved?]
  (let [look (active-look e (long t))
        [dyaw dpitch] (look-angles world e height look)
        hy (v/limit-angle (double (or (:head-yaw e) (:yaw e))) (double dyaw) 10.0)
        hp (v/limit-angle (double (or (:pitch e) 0.0)) (double dpitch) 40.0)
        e (if (head-same? e look (double hy) (double hp))
            e
            (entity/mob-looked e hy hp look))]
    (control/body-tick e moved? (long t))))

(def ^:private rest-vel (v/v3 0.0 (* 0.98 (- 0.0 0.08)) 0.0))

(declare physics-move)

(defn- dead-band ^double [^double a]
  (if (< (Math/abs a) 0.005) 0.0 a))

(defn- at-rest? [world e half moving? water? [vx0 vy0 vz0] [cx cz]]
  (let [pos (:pos e)]
    (and (not moving?) (boolean (:on-ground e)) (not water?)
         (< (^[double] Math/abs (+ (double vx0) (double cx))) 0.005)
         (< (^[double] Math/abs (+ (double vz0) (double cz))) 0.005)
         (<= -0.0785 (double vy0) 0.0)
         (phys/standing-on-cubes? (:chunks world)
                                  (v/x pos) (v/y pos) (v/z pos) half))))

(defn- rest-step [world e height t]
  (head-update world
               (entity/mob-moved e (:pos e) rest-vel true (:yaw e) false (:jump-cd e))
               height t false))

(defn- lava-flagged
  "The mob with :in-lava? fresh: isInLava of the tick just walked."
  [e lava?]
  (if (= (boolean (:in-lava? e)) lava?) e (assoc e :in-lava? lava?)))

(defn- physics [world index eid e half height]
  (let [t (long (:tick world))
        water? (in-water? world (:pos e) height)
        moving? (not (zero? (double (:zza (:move e) 0.0))))
        push (push/push index eid e t half height)
        vel0 (:vel e)
        vel [(dead-band (v/x vel0)) (v/y vel0) (dead-band (v/z vel0))]]
    (lava-flagged
      (if (at-rest? world e half moving? water? vel push)
        (rest-step world e height t)
        (physics-move world eid e water? vel push half height moving?))
      (in-lava? world (:pos e) height))))

(defn- water-push [world e half height water?]
  (if water?
    (liquid/entity-push (:chunks world) (:pos e) half height (:vel e))
    zero3))

(defn- steer-axis ^double [^double v0 h ^double accel]
  (+ v0 (if h (* (double h) accel) 0.0)))

(defn- drop-axis ^double [drop? ^double a]
  (if (and drop? (< (Math/abs a) 0.005)) 0.0 a))

(defn- swim-bob ^double [^long t ^long eid]
  (if (< (random/of-longs t eid (hash :swim)) 0.8) 0.04 0.0))

(defn- steer-vel [world e t eid water? [hx hz] [vx0 vy0 vz0] [cx cz] half height moving?]
  (let [mv (:move e)
        og (boolean (:on-ground e))
        accel (* (double (:zza mv 0.0))
                 (if (and og (not water?))
                   (double (:speed mv 0.0))
                   air-accel))
        wpush (water-push world e half height water?)
        drop? (and (not moving?) (not water?))
        ax (+ (steer-axis (double vx0) hx (double accel)) (double cx) (v/x wpush))
        az (+ (steer-axis (double vz0) hz (double accel)) (double cz) (v/z wpush))]
    (v/v3 (drop-axis drop? ax)
          (if water?
            (+ (double vy0) (v/y wpush) (swim-bob (long t) (long eid)))
            (double vy0))
          (drop-axis drop? az))))

(defn- bumped? [moving? vx vz nx nz]
  (and moving? (or (and (not (zero? (double vx))) (zero? (double nx)))
                   (and (not (zero? (double vz))) (zero? (double nz))))))

(defn- next-vy [world e ny water? bump? jump?]
  (let [ny (double ny)]
    (cond (and bump? water? (water-above? world (:pos e))) 0.3
          jump? jump-speed
          water? (- (* water-friction ny) 0.02)
          :else (* 0.98 (- ny gravity)))))

(defn- friction ^double [og water?]
  (cond water? water-friction og ground-friction :else air-friction))

(defn- jump-now? [e ^Move mv t]
  (and (.on-ground mv) (boolean (:jump e))
       (>= (long t) (long (or (:jump-cd e) 0)))))

(defn- bubbled-vy [world e ^Move mv water? bump? jump?]
  (liquid/bubble-push (:chunks world) (.pos mv)
                      (next-vy world e (v/y (.vel mv)) water? bump? jump?)))

(defn- mob-stepped [e ^Move mv ny fric water? jump? t]
  (let [vel (.vel mv)]
    (entity/mob-moved e (.pos mv)
                      (v/v3 (* (double (v/x vel)) (double fric)) (double ny)
                            (* (double (v/z vel)) (double fric)))
                      (.on-ground mv) (:yaw e) water?
                      (if jump? (+ (long t) 10) (:jump-cd e)))))

(defn- shifted?
  "Whether the mob moved far enough this tick to carry its body."
  [from to]
  (let [dx (- (v/x to) (v/x from)) dz (- (v/z to) (v/z from))]
    (> (+ (* dx dx) (* dz dz)) 2.5000003E-7)))

(defn- physics-move [world eid e water? vel0 push half height moving?]
  (let [t (long (:tick world))
        from (:pos e)
        fric (friction (boolean (:on-ground e)) water?)
        drive (steer-vel world e t eid water? (heading e)
                         vel0 push half height moving?)
        ^Move mv (phys/move (:chunks world) (:pos e) drive half height 0.6)
        vel (.vel mv)
        bump? (bumped? moving? (v/x drive) (v/z drive) (v/x vel) (v/z vel))
        jump? (jump-now? e mv t)
        ny (bubbled-vy world e mv water? bump? jump?)
        e (mob-stepped e mv ny fric water? jump? t)]
    (head-update world e height t (shifted? from (:pos e)))))

(def ^:private ^:const say-rest 120)

(def ^:private ^:const say-mean 40)

(defn- sound-pitch ^double [e ^long t ^long eid]
  (let [base (if (mobs/baby? e) 1.5 1.0)]
    (+ base (* 0.2 (- (random/of-longs t eid (hash :p1))
                      (random/of-longs t eid (hash :p2)))))))

(defn- wide-pitch ^double [^long t ^long eid kind]
  (+ 1.0 (* 0.4 (- (random/of-longs t eid (hash kind) (hash :w1))
                   (random/of-longs t eid (hash kind) (hash :w2))))))

(defn- water-vol ^double [vel3 k]
  (let [vx (v/x vel3) vy (v/y vel3) vz (v/z vel3)]
    (min 1.0 (* (Math/sqrt (+ (* vx vx 0.2) (* vy vy) (* vz vz 0.2)))
                (double k)))))

(defn- next-say ^long [^long t ^long eid]
  (+ t say-rest (mobs/exp-delay say-mean t eid :say)))

(defn- ambient [eid e t]
  (let [t (long t) eid (long eid) st (:say-tick e)]
    (if (and st (< t (long st)))
      [e nil]
      (if-let [say (mobs/sound-of e :say)]
        (if (nil? st)
          [(assoc e :say-tick (next-say t eid)) nil]
          [(assoc e :say-tick (next-say t eid))
           [(out/all (out/sound say (:pos e) 1.0 (sound-pitch e t eid)))]])
        [e nil]))))

(defn- step-sound-delta [e t eid]
  (if (:wet? e)
    (out/all (out/sound :swim (:pos e)
                        (water-vol (:vel e) 0.35)
                        (wide-pitch (long t) (long eid) :swm)))
    (when (:on-ground e)
      (when-let [snd (mobs/sound-of e :step)]
        (out/all (out/sound snd (:pos e) 0.15 1.0))))))

(defn- movement-sounds [acc e was-wet? old-walked new-walked t eid]
  (let [acc (if (and (:wet? e) (not was-wet?))
              (conj acc (out/all (out/sound :splash (:pos e)
                                            (water-vol (:vel e) 0.2)
                                            (wide-pitch (long t) (long eid) :spl))))
              acc)]
    (if (> (long (Math/floor (double new-walked)))
           (long (Math/floor (double old-walked))))
      (if-let [d (step-sound-delta e t eid)] (conj acc d) acc)
      acc)))

(defmacro ^:private diff-fields
  [old new & ks]
  (let [o (with-meta (gensym) {:tag 'collider.game.entity.Mob})
        n (with-meta (gensym) {:tag 'collider.game.entity.Mob})]
    `(let [~o ~old ~n ~new]
       (cond-> {}
               ~@(mapcat (fn [k]
                           (let [f (symbol (str ".-" (name k)))]
                             [`(not (identical? (~f ~n) (~f ~o)))
                              `(assoc ~k (~f ~n))]))
                         ks)))))

(def ^:private loose-keys
  [:nav :move :jump :body :follow-at :in-lava? :float?])

(defn- mob-changes
  "The fields of a mob that one tick left different.
  The controls live outside the record, so they are compared by
  hand."
  [old new]
  (reduce (fn [m k]
            (if (identical? (k old) (k new)) m (assoc m k (k new))))
          (diff-fields old new
                       :pos :vel :yaw :pitch :on-ground :task :follow
                       :no-action :baby-until :tempt-cooldown-until
                       :say-tick :walked :head-yaw :look :jump-cd :wet?
                       :sheared?)
          loose-keys))

(defn- age-up [e t]
  (if (and (mobs/baby? e) (>= (long t) (long (:baby-until e))))
    (assoc e :baby-until nil)
    e))

(defn- brain-step [world eid e t tempters dead?]
  (let [[e1 deltas] (if dead? [e nil] (think world eid e t tempters))
        e1 (age-up e1 t)
        [e1 say-deltas] (if dead? [e1 nil] (ambient eid e1 t))]
    [e1 deltas say-deltas]))

(defn- walked-step [e prev now]
  (let [walked (double (or (:walked e) 0.0))
        walked' (+ walked (* 0.6 (Math/sqrt (v/dist3-sq (:pos prev) (:pos now)))))]
    [(if (== walked walked') now (assoc now :walked walked')) walked walked']))

(defn- controlled
  "The mob after its navigation and controls have run.
  Vanilla order: the navigation aims the move control in the same
  tick the move control reads it, and both run every tick."
  [world e speed half]
  (control/tick world (nav/tick world e) speed (* 2.0 (double half))))

(defn- step-mob [world index tempters eid e t]
  (let [[half height] (mobs/box-of e)
        speed (get-in mobs/types [(:type e) :speed])
        dead? (not (pos? (double (:health e))))
        e0 (cond-> e (:jump e) (assoc :jump false))
        [e1 deltas say-deltas] (brain-step world eid e0 t tempters dead?)
        e1 (if dead? e1 (controlled world e1 speed half))
        was-wet? (boolean (:wet? e))
        e2 (physics world index eid e1 half height)
        [e2 walked walked'] (walked-step e e1 e2)
        changes (mob-changes e e2)]
    (-> (if (seq changes) [[:merge-entity eid changes]] [])
        (cond-> deltas (into deltas)
                say-deltas (into say-deltas))
        (movement-sounds e2 was-wet? walked walked' t eid))))

(defn mobs-system [world d]
  (let [events (:input d)
        t (long (:tick world))
        active (state/active-chunks world)
        index (push/push-index world active)
        tempters (sense/holders world)
        herd (into []
                   (filter (fn [[_ e]] (and (mobs/mob-type? (:type e))
                                            (state/active-at? active (:pos e)))))
                   (:entities world))]
    (conj (mapv (fn [batch]
                  #(into [] (mapcat (fn [me] (step-mob world index tempters (key me) (val me) t))) batch))
                (partition-all 32 herd))
          #(interact-deltas world events t))))
