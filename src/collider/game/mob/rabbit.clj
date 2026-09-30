(ns collider.game.mob.rabbit
  "Rabbit hops, goals, garden raids and kits."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.size :as size]
            [collider.game.game-mode :as game-mode]
            [collider.game.mob.animal :as animal]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.game.mob.nav :as nav]
            [collider.game.mob.randompos :as pos]
            [collider.game.mob.sense :as sense]
            [collider.game.out :as out]
            [collider.game.systems.blocks.edit :as edit]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.grow.common :as grow]
            [collider.world.blocks.grow.crop :as crop]
            [collider.world.env.biome :as biome]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.env.signal :as signal])
  (:import (collider.game.mob Steer)))

(set! *warn-on-reflection* true)

(def ^:private ^:const hop-ticks 15)

(def ^:private ^:const hop-delay 10)

(def ^:private ^:const panic-hop-delay 3)

(def ^:private ^:const flee-speed 2.2)

(def ^:private ^:const swim-speed 1.5)

(def ^:private ^:const slow-hop 0.6)

(def ^:private ^:const carrot-wait 40)

(def ^:private ^:const kit-chance 20)

(def ^:private hop-push (double (float 0.1)))

(def ^:private jump-strength (double (float 0.42)))

(def ^:private origin (Steer/wanted nil 0.0 0.0 0.0 0.0))

(defn- hop [e] (or (:hop e) {}))

(defn- move-of ^Steer [e] (Steer/of (or (:move e) origin)))

(defn- rewanted [m ^double s]
  (Steer/wanted m (double (:x m)) (double (:y m)) (double (:z m)) s))

(defn- wanted-speed ^double [e ^double s]
  (if (:wet? e) swim-speed s))

(defn- next-speed
  "RabbitMoveControl.setWantedPosition: a rabbit in water hops at
  1.5, and each positive speed is the speed of its next hop."
  [e ^double s]
  (let [h (hop e)]
    (if (and (pos? s) (not= s (:next h))) (assoc h :next s) h)))

(defn- nav-speed [e ^double s]
  (when-let [n (:nav e)]
    (if (== s (double (:speed n 0.0))) n (assoc n :speed s))))

(defn- speed-set
  "Rabbit.setSpeedModifier: the navigation and the move control of
  rabbit e take speed s, and the control keeps what it aims at."
  [e ^double s]
  (let [m (move-of e) w (wanted-speed e s)]
    (entity/with e {:nav (nav-speed e s)
                    :move (rewanted m w)
                    :hop (next-speed e w)})))

(defn- aimed-at
  "Rabbit e after its navigation told it where to go with move m."
  [e nav ^Steer m]
  (let [w (wanted-speed e (double (:mult m)))
        m (if (== w (double (:mult m))) m (rewanted m w))]
    (entity/with e {:nav nav :move m :hop (next-speed e w)})))

(defn- pitch ^double [t eid]
  (let [r #(double (float (animal/rnd t eid [:hop-pitch %])))
        d (float (- (r 1) (r 2)))
        p (float (+ (float (* d (float 0.2))) (float 1.0)))]
    (double (float (* p (float 0.8))))))

(defn- start-jumping
  "Rabbit.startJumping: rabbit e jumps as soon as it may, and its hop
  lasts 15 ticks; the jump sounds from where it stands."
  [e]
  (assoc e :hop (assoc (hop e) :jumping? true :duration hop-ticks
                       :ticks 0 :sound-at (:pos e))))

(defn- faced ^double [e ^double x ^double z]
  (let [p (:pos e)
        a (* (Steer/atan2 (- z (v/z p)) (- x (v/x p))) 180.0)]
    (double (float (- (float (/ a (double (float Math/PI))))
                      (float 90.0))))))

(defn- hop-target [e ^Steer m]
  (let [nv (:nav e)]
    (if (nav/done? e)
      [(:x m) (:z m)]
      (let [n (nth (:nodes (:path nv)) (long (:index nv)))
            off (* 0.5 (long (+ (* 2.0 (double (first (mobs/box-of e))))
                                1.0)))]
        [(+ (double (:x n)) off) (+ (double (:z n)) off)]))))

(defn- hop-off [e]
  (let [m (move-of e)]
    (if (and (identical? Steer/MOVE_TO (:op m))
             (zero? (long (:delay (hop e) 0))))
      (let [[x z] (hop-target e m)]
        (start-jumping (assoc e :yaw (faced e x z))))
      e)))

(defn- landed [e]
  (let [slow? (< (double (:mult (move-of e))) flee-speed)]
    (update e :hop assoc :jumping? false
            :delay (if slow? hop-delay panic-hop-delay))))

(defn- delay-down [h]
  (let [c (long (:delay h 0))] (if (pos? c) (assoc h :delay (dec c)) h)))

(defn- carrots-down [h t eid]
  (let [c (long (:carrots h 0))]
    (if (pos? c)
      (let [n (long (* 3.0 (animal/rnd t eid :carrot-wait)))]
        (assoc h :carrots (max 0 (- c n))))
      h)))

(defn- own-step
  "Rabbit.customServerAiStep for rabbit e: it counts its waits down,
  pauses when it lands, and hops toward where it aims when it may."
  [eid e t]
  (let [h (-> (hop e) delay-down (carrots-down t eid))
        og? (boolean (:on-ground e))
        e (cond-> (assoc e :hop h)
            (and og? (not (:was-on-ground? h))) landed
            og? hop-off)]
    (if (= og? (boolean (:was-on-ground? (hop e))))
      e
      (assoc-in e [:hop :was-on-ground?] og?))))

(defn- move-control
  "RabbitMoveControl.tick: a rabbit at rest on the ground stops, one
  in its hop moves at the speed of the hop, then the control acts."
  [world e attr width]
  (let [h (hop e) m (move-of e)
        e (cond
            (and (:on-ground e) (not (:jumping? h))) (speed-set e 0.0)
            (or (identical? Steer/MOVE_TO (:op m))
                (identical? Steer/JUMPING (:op m)))
            (speed-set e (double (:next h 0.0)))
            :else e)]
    (control/tick world e attr width)))

(defn- jump-control
  "RabbitJumpControl.tick: a jump the move control asked for starts a
  hop. The rabbit jumps while its hop lasts."
  [e]
  (let [e (if (:jump e) (start-jumping e) e)
        j (boolean (:jumping? (hop e)))]
    (if (= j (boolean (:jump e))) e (entity/with e {:jump j}))))

(defn steered
  "Returns rabbit e after its navigation, its own step and its move
  and jump controls, for speed attribute attr and half width half."
  [world eid e attr half]
  (let [[e nav m] (nav/aim world e)
        e (if m (aimed-at e nav m) e)
        e (own-step eid e (:tick world))]
    (jump-control
      (move-control world e attr (* 2.0 (double half))))))

(defn- next-node-y [e]
  (when-not (nav/done? e)
    (let [nv (:nav e)]
      (:y (nth (:nodes (:path nv)) (long (:index nv)))))))

(defn- high? [y e]
  (and y (> (double y) (+ (v/y (:pos e)) 0.5))))

(defn jump-share
  "Rabbit.getJumpPower: the share of its jump strength rabbit e jumps
  with. A hop up to a higher node or against a wall is the highest, a
  slow hop the lowest."
  ^double [e]
  (let [m (move-of e) h (hop e)
        base (cond (or (:hit? h)
                       (and (:jumping? h) (high? (:y m) e))
                       (high? (next-node-y e) e)) (float 0.5)
                   (<= (double (:mult m)) slow-hop) (float 0.2)
                   :else (float 0.3))]
    (double (float (/ (float base) (float jump-strength))))))

(defn- push-of [e]
  (let [up (if (mobs/baby? e) 0.5 1.5)
        d (Math/sqrt (+ (* up up) 1.0))
        r (double (float (* (double (float (:yaw e)))
                            (double (float (/ Math/PI 180.0))))))]
    [(* (/ up d) hop-push) (* (/ 1.0 d) hop-push)
     (double (float (v/sin r))) (double (float (v/cos r)))]))

(defn hopped
  "Rabbit.jumpFromGround: a rabbit that hops with a speed but hardly
  moves gets a push forward and up."
  [e vel]
  (if (and (pos? (double (:mult (move-of e))))
           (< (+ (* (v/x vel) (v/x vel)) (* (v/z vel) (v/z vel))) 0.01))
    (let [[my mz s c] (push-of e)
          my (double my) mz (double mz) s (double s) c (double c)]
      (v/v3 (+ (v/x vel) (- (* 0.0 c) (* mz s)))
            (+ (v/y vel) my)
            (+ (v/z vel) (+ (* mz c) (* 0.0 s)))))
    vel))

(defn- collided? [d u]
  (or (>= (Math/abs (- (v/x d) (v/x u))) (double (float 1.0E-5)))
      (>= (Math/abs (- (v/z d) (v/z u))) (double (float 1.0E-5)))))

(defn bumped
  "Returns rabbit e that met a wall in its move this tick or did not,
  as Entity.move sees it from the move d it asked for and the move u
  it made."
  [e d u]
  (let [hit? (and d (collided? d u))]
    (if (= hit? (boolean (:hit? (hop e))))
      e
      (entity/with e {:hop (assoc (hop e) :hit? hit?)}))))

(defn- aged-hop [h]
  (let [n (long (:ticks h 0)) d (long (:duration h 0))]
    (cond (not= n d) (assoc h :ticks (inc n))
          (not (zero? d)) (assoc h :ticks 0 :duration 0 :jumping? false)
          :else h)))

(defn- hop-deltas [eid e t h]
  (cond-> []
    (:sound-at h)
    (conj (out/all (out/sound :rabbit/jump (:sound-at h) 1.0
                              (pitch t eid))))
    (= (:jump-cd e) (+ (long t) 10))
    (conj (out/all (out/status eid :hop)))))

(defn ai-step
  "Returns rabbit e after the part of its step that is its own, and
  its deltas: its hop ages, and a jump sounds and shows."
  [eid e t]
  (let [h (hop e)
        ds (hop-deltas eid e t h)
        h' (aged-hop (dissoc h :sound-at))]
    [(if (= h h') e (entity/with e {:hop h'})) (not-empty ds)]))

(def ^:private panic
  (assoc (animal/goal :panic)
    :tick (fn [_ _ _ e _ _]
            [(speed-set e (animal/goal-speed e :panic)) nil])))

(defn- enemy-player? [world o]
  (and (= :player (:type o)) (game-mode/seen? o)
       (not (game-mode/may-fly? o))
       (pos? (difficulty/id world))))

(defn- wolf? [_ o] (= :wolf (:type o)))

(defn- monster? [_ o] (boolean (get-in mobs/types [(:type o) :monster?])))

(defn- boxed? [e o]
  (let [[_ h] (mobs/box-of e) [_ oh] (size/box o)
        y (v/y (:pos e)) oy (v/y (:pos o))]
    (and (< oy (+ y (double h) 3.0)) (> (+ oy (double oh)) (- y 3.0)))))

(defn- dreaded? [world e fear r oid o]
  (and (not= :evil (mobs/rabbit-variants (:color e)))
       (fear world o) (boxed? e o)
       (sense/in-range? (:pos e) o r)
       (animal/in-sight? world e o)))

(defn- dreaded [world e fear ^double r]
  (let [pred #(dreaded? world e fear r %1 %2)
        r2 (let [m (max r 2.0)] (* m m))]
    (sense/nearest world (:pos e) r2 pred)))

(defn- away-pos [world eid e t o]
  (let [p (:pos e) q (:pos o)]
    (pos/pos-away world e t eid :avoid 16 7
                  (- (v/x p) (v/x q)) (- (v/z p) (v/z q)))))

(defn- fled [world eid e t kind oid o]
  (when-let [cell (away-pos world eid e t o)]
    (when (>= (v/dist3-sq (:pos o) cell) (v/dist3-sq (:pos o) (:pos e)))
      (let [speed (animal/goal-speed e :avoid)]
        (some-> (nav/path-to world e cell speed 0)
                (assoc :task {:kind kind :from oid})
                (vector nil))))))

(defn- avoid
  "RabbitAvoidEntityGoal: flees what it fears within r, unless it is
  the killer bunny."
  [kind fear r]
  {:kind kind :flags #{:move} :prio 4
   :start (fn [world eid e t _]
            (when-let [[_ oid o] (dreaded world e fear r)]
              (fled world eid e t kind oid o)))
   :continue? (fn [_ e _ _] (not (nav/done? e)))
   :tick (fn [_ _ _ e _ _]
           [(entity/with e {:nav (nav-speed e (animal/goal-speed
                                                e :avoid))})
            nil])})

(def ^:private raid-scan
  (let [order (fn [from r] (take-while #(<= (long %) r)
                                        (iterate #(if (pos? (long %))
                                                    (- (long %))
                                                    (- 1 (long %)))
                                                 from)))]
    (vec (for [y (order 0 1) r (range 16) x (order 0 r)
               z (order (if (< (- r) x r) r 0) r)]
           [x (dec (long y)) z]))))

(defn- carrot-at? [world [x y z]]
  (= :carrot (block/type-of (sense/block-at world x (inc (long y)) z))))

(defn- ripe-carrot? [world [x y z]]
  (and (carrot-at? world [x y z])
       (>= (grow/age (sense/block-at world x (inc (long y)) z))
           (long (crop/max-age :carrot)))))

(defn- valid-target? [world h cell]
  (and (block/tagged? (sense/block-at world cell) "supports_crops")
       (:wants? h) (not (:can-raid? h))
       (ripe-carrot? world cell)))

(defn- garden [world e h]
  (let [[x y z] (sense/feet-cell (:pos e))]
    (some (fn [[dx dy dz]]
            (let [c [(+ (long x) (long dx)) (+ (long y) (long dy))
                     (+ (long z) (long dz))]]
              (when (valid-target? world h c) c)))
          raid-scan)))

(defn- raid-wait ^long [t eid]
  (quot (inc (+ 200 (long (* 200.0 (animal/rnd t eid :raid-wait)))))
        2))

(defn- stay ^long [t eid]
  (let [n (long (* 1200.0 (animal/rnd t eid [:raid-stay 1])))]
    (+ 1200 (long (* (+ n 1200) (animal/rnd t eid [:raid-stay 2]))))))

(defn- raid-start [world eid e t h]
  (let [wait (raid-wait t eid)
        cell (garden world e h)
        h (assoc h :raid-wait wait :can-raid? (some? cell))
        e (entity/with e {:hop h})]
    (if-let [[x y z] cell]
      [(assoc (nav/move-to world e [x (inc (long y)) z]
                           (animal/goal-speed e :raid))
         :task {:kind :raid :cell cell :tries 0 :stay (stay t eid)})
       nil]
      [e nil false])))

(defn- raid-can-use [world eid e t _]
  (let [h (hop e) wait (long (:raid-wait h 0))]
    (cond
      (and (<= wait 0) (not (get-in world [:rules :mob-griefing] true)))
      nil
      (pos? wait) [(assoc e :hop (assoc h :raid-wait (dec wait))) nil false]
      :else (raid-start world eid e t
                        (assoc h :can-raid? false
                               :wants? (<= (long (:carrots h 0)) 0))))))

(defn- raid-going? [world e _ _]
  (let [h (hop e) {:keys [tries stay cell]} (:task e)]
    (and (:can-raid? h) (<= (- (long stay)) (long tries) 1200)
         (valid-target? world h cell))))

(defn- centre-dist-sq ^double [[x y z] p]
  (v/dist3-sq [(+ (long x) 0.5) (+ (long y) 1.5) (+ (long z) 0.5)] p))

(defn- walked-on [world e [x y z] tries]
  (let [e (assoc-in e [:task :tries] tries)]
    (if (zero? (rem (long tries) 40))
      (nav/move-to world e [x (inc (long y)) z]
                   (animal/goal-speed e :raid))
      e)))

(defn- eaten [world eid [x y z]]
  (let [crop [x (inc (long y)) z]
        st (sense/block-at world crop)
        a (grow/age st)]
    (if (zero? a)
      (edit/flagged-deltas world [[crop 0 []]] 2)
      (concat (edit/flagged-deltas
                world [[crop (grow/aged st (dec a)) [[:break st]]]] 2)
              (signal/game-event :block-change crop eid)))))

(defn- raided [world eid e cell]
  (let [h (hop e)
        ok? (and (:can-raid? h) (carrot-at? world cell))
        h (cond-> (assoc h :can-raid? false :raid-wait 10)
            ok? (assoc :carrots carrot-wait))]
    [(entity/with e {:hop h}) (when ok? (eaten world eid cell))]))

(defn- raid-tick [_ world eid e t _]
  (let [{:keys [cell tries]} (:task e)
        [x y z] cell
        near? (< (centre-dist-sq cell (:pos e)) 1.0)
        e (if near?
            (assoc-in e [:task :tries] (dec (long tries)))
            (walked-on world e cell (inc (long tries))))
        e (assoc e :look {:at [(+ (long x) 0.5) (inc (long y))
                               (+ (long z) 0.5)]
                          :until (+ (long t) 2)})]
    (if near? (raided world eid e cell) [e nil])))

(def ^:private raid
  {:kind :raid :flags #{:move :jump} :prio 5 :every-tick? true
   :start raid-can-use :continue? raid-going? :tick raid-tick})

(defn raiding?
  "Returns true when rabbit e may eat a carrot at tick t, the one way
  a rabbit changes a block."
  [e _t]
  (= :raid (get-in e [:task :kind])))

(def ^:private climb
  {:kind :climb :flags #{:jump} :prio 1 :every-tick? true
   :start (fn [_ _ e _ _] (when (:in-powder-snow? e) [e nil]))
   :continue? (fn [_ e _ _] (boolean (:in-powder-snow? e)))
   :tick (fn [_ _ _ e _ _] [(assoc e :jump true) nil])})

(defn- kit-variant [world t eid a b]
  (let [v (mobs/rabbit-variant [t eid :kit] (biome/at (:dim world) nil))]
    (cond (animal/one-in? t eid :kit kit-chance) v
          (< (animal/rnd t eid :kit-parent) 0.5) (:color b)
          :else (:color a))))

(def spec
  "The goals of a rabbit.
  A kit takes the variant of either parent, or one in twenty times the
  variant of the biome."
  (animal/spec
    [(assoc (animal/goal :float) :prio 1)
     climb
     (assoc panic :prio 1)
     (assoc (animal/goal :mate) :prio 2)
     (assoc (animal/goal :tempt) :prio 3)
     (avoid :avoid-player enemy-player? 8.0)
     (avoid :avoid-wolf wolf? 10.0)
     (avoid :avoid-monster monster? 4.0)
     raid
     (assoc (animal/goal :wander) :prio 6)
     (assoc (animal/goal :look-player) :prio 11)]
    kit-variant))

(defn brain
  "Returns the rabbit's next state and deltas for one tick."
  [world eid e t tempters]
  (animal/brain spec world eid e t tempters))
