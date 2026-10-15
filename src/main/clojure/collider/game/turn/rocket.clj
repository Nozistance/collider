(ns collider.game.turn.rocket
  "The turn of a firework rocket. It speeds up as it climbs, slides
  along what stops it and goes off when its life runs out or it meets
  a body. A rocket set off by a glider rides at its hand and speeds it
  up along its look."
  (:require [collider.game.entity :as entity]
            [collider.game.glide :as glide]
            [collider.game.player :as player]
            [collider.game.entity.hurt :as hurt]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.projectile :as projectile]
            [collider.game.stack :as stack]
            [collider.num :as num]
            [collider.parallel :as par]
            [collider.random :as random]
            [collider.world.direction :as dir]
            [collider.vec :as v]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(defn- half ^double [] (size/half :firework-rocket))

(defn- height ^double [] (size/height :firework-rocket))

(defn- sped-up [e]
  (let [k (if (:horizontal-collision e) 1.0 1.15)]
    [(* (double (v/x (:vel e))) k) (+ (double (v/y (:vel e))) 0.04)
     (* (double (v/z (:vel e))) k)]))

(defn- moved [world e d]
  (let [stuck (:stuck e)
        m (if stuck (mapv * d stuck) d)
        mv (phys/move (:chunks world) (:pos e) m (half) (height))
        at (phys/pos mv)]
    {:pos at :on-ground (phys/on-ground? mv)
     :horizontal-collision (or (not= (v/x m) (v/x (phys/vel mv)))
                (not= (v/z m) (v/z (phys/vel mv))))
     :stuck (motion/stuck-speed (:chunks world) [[(:pos e) at m]]
                                (half) (height))}))

(defn- stars ^long [e]
  (count (:explosions (stack/component (:stack e) :fireworks))))

(defn- source [world eid e at]
  (let [o (some->> (:owner e) (get (:entities world)))]
    {:type :fireworks :cause (when o (:owner e)) :direct eid :from at
     :player? (= :player (:type o)) :attacker o :direct-attacker e}))

(defn- seen? [world at t]
  (let [h (second (entity/box t))]
    (some (fn [^double k]
            (let [[x y z] (:pos t)
                  to [x (+ (double y) (* h k)) z]]
              (nil? (projectile/block-clip world at (mapv - to at)))))
          [0.0 0.5])))

(defn- near [world at cid]
  (for [[oid t] (par/keyed (:entities world))
        :when (and (not= oid cid) (projectile/hittable? t)
                   (not (game-mode/spectator? t))
                   (<= (v/dist-sq at (:pos t)) 25.0)
                   (seen? world at t))]
    [oid t]))

(defn- distance
  "Returns how far t is from at, in floats as an entity measures it."
  ^double [at t]
  (let [[dx dy dz] (map #(num/f32 (- (double %1) (double %2))) at (:pos t))
        sq (num/f32 (+ (num/f32 (+ (num/f32 (* dx dx)) (num/f32 (* dy dy))))
                       (num/f32 (* dz dz))))]
    (num/f32 (Math/sqrt sq))))

(defn- hit-deltas [world src [oid t hit]]
  (when-let [ds (hurt/damage-deltas world oid t hit src)]
    (into ds (hurt/report-deltas
               world oid (hurt/hurt-now world oid t ds)))))

(defn- blast-deltas
  "Returns the deltas of rocket e of eid going off at at. Its carrier
  takes the whole blast, the bodies around less with distance."
  [world eid e at]
  (let [n (stars e)]
    (when (pos? n)
      (let [amount (num/f32 (+ 5.0 (* 2.0 n)))
            src (source world eid e at)
            cid (:attached e)
            c (get-in world [:entities cid])
            around (for [[oid t] (near world at cid)
                         :let [d (distance at t)
                               k (Math/sqrt (/ (- 5.0 d) 5.0))]]
                     [oid t (num/f32 (* amount (num/f32 k)))])]
        (mapcat #(hit-deltas world src %)
                (cond->> around c (cons [cid c amount])))))))

(defn- explosion [world eid e at]
  (concat [(out/all (out/status eid :fireworks))]
          (blast-deltas world eid e at)
          [[:remove-entity eid]]))

(defn- launch [e at]
  (when (zero? (long (:life e 0)))
    [(out/all (out/sound :firework-rocket/launch at 3.0 1.0))]))

(defn- free-motion [world e]
  (let [d (if (:shot-at-angle? e) (:vel e) (sped-up e))]
    [(:pos e) d (assoc (moved world e d) :vel d) nil]))

(defn- hand
  "Returns the offset from the feet of glider c to the hand that
  holds its rocket."
  [c]
  (let [held? #(= :firework-rocket (:item (player/hand-stack c %)))
        off? (and (held? :off) (not (held? :main)))
        right? (not= off? (= :right (:main-hand c :right)))
        turn (if right? 80.0 -80.0)
        yaw (num/f32 (+ (num/f32 (:yaw c 0.0)) turn))]
    (mapv #(* 0.5 (double %)) (glide/look 0.0 yaw))))

(defn- carrier
  "Returns what rocket e knows of its carrier, where it stands, its
  motion, its look, the hand that holds the rocket and whether it
  glides. A carrier gone from the world stays as it left, or as the
  rocket last knew it."
  [world e]
  (if-let [c (or (get-in world [:entities (:attached e)])
                 (some #(when (= (:attached e) (:eid %)) %)
                       (get-in world [:input :quits])))]
    {:pos (:pos c) :vel (:vel c [0.0 0.0 0.0])
     :look (glide/look (double (:pitch c 0.0)) (double (:yaw c 0.0)))
     :hand (hand c) :fall-flying (:fall-flying c) :here? true}
    (:carrier e)))

(defn- carried-motion
  "Returns where rocket e rides on its carrier, its motion, the
  changes to merge into it and the boost it gives a gliding carrier."
  [world e]
  (let [{:keys [pos vel look here?] :as c} (carrier world e)
        kept #(assoc (dissoc c :here?) :vel %)]
    (cond
      (nil? c) [(:pos e) (:vel e) {} nil]
      (:fall-flying c)
      (let [at (mapv + pos (:hand c))
            vel (glide/boosted vel look)]
        [at vel {:pos at :vel vel :carrier (kept vel)}
         (when here? [[:boost (:attached e) look]])])
      :else [pos vel {:pos pos :vel vel :carrier (kept vel)} nil])))

(defn- goes-off? [e hit]
  (or (= :entity (:kind hit))
      (and (= :block (:kind hit)) (pos? (stars e)))))

(defn- flew-deltas [eid e m d left?]
  (let [[yaw pitch] (projectile/rotated e d)]
    [[:merge-entity eid
      (assoc m :left-owner? left? :yaw yaw :pitch pitch
             :life (inc (long (:life e 0)))
             :age (inc (long (:age e 0))))]]))

(defn step-deltas
  "Returns the deltas of the turn of firework rocket e of eid."
  [world eid e]
  (if (< (v/y (:pos e)) (chunk/void-y world))
    [[:remove-entity eid]]
    (let [left? (projectile/left-owner? world e (:vel e))
          [from d m boost] (if (:attached e)
                             (carried-motion world e)
                             (free-motion world e))
          placed (assoc e :pos from :left-owner? left?)
          hit (projectile/clip world eid placed d)
          met? (goes-off? e hit)
          at (:pos m from)]
      (concat
        boost
        (when met? (explosion world eid e at))
        (when (= :block (:kind hit))
          (projectile/block-hit-deltas world e (:cell hit)))
        (launch e at)
        (cond met? nil
              (>= (long (:life e 0)) (long (:lifetime e 0)))
              (explosion world eid e at)
              :else (flew-deltas eid e m d left?))))))

(defn- draw ^double [world eid k] (random/of-key (:tick world) eid [:rocket k]))

(defn- drift ^double [world eid k]
  (* 0.002297 (- (draw world eid [k 0]) (draw world eid [k 1]))))

(defn rocket
  "Returns a new rocket of stack that player eid sets off at at."
  [world eid stack at]
  (let [flight (long (:flight-duration
                       (stack/component stack :fireworks) 0))]
    {:type :firework-rocket :owner eid :left-owner? false :age 0 :life 0
     :lifetime (+ (* 10 (inc flight)) (random/below (draw world eid :a) 6)
                  (random/below (draw world eid :b) 7))
     :pos at :vel [(drift world eid :x) 0.05 (drift world eid :z)]
     :yaw 0.0 :pitch 0.0 :on-ground false :stack stack}))

(defn launch-deltas
  "Returns the deltas of player eid launching the rocket in hand from
  face of the block at pos, clicked at cursor in sixteenths."
  [world [eid pos face _ cursor]]
  (let [e (get-in world [:entities eid])
        stack (player/hand-stack e (:use-hand e))
        [ox oy oz] (dir/face-offset face)
        at (mapv (fn [p c o] (+ (+ (double p) (/ (double c) 16.0))
                                (* (double o) 0.15)))
                 pos cursor [ox oy oz])]
    [[:spawn-entity (rocket world eid stack at)]]))

(defn boost-deltas
  "Returns the deltas of player eid, e, setting off the rocket in hand
  on itself, or nil when it does not glide."
  [world eid e]
  (when (:fall-flying e)
    (let [stack (player/hand-stack e (:use-hand e))]
      [[:spawn-entity
        (assoc (rocket world eid stack (:pos e)) :attached eid)]])))
