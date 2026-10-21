(ns collider.game.block.piston-push
  "The pushes a moving piston gives the bodies in its way."
  (:require [collider.game.attribute :as attribute]
            [collider.game.entity :as entity]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.sections :as sections]
            [collider.game.entity.size :as size]
            [collider.game.hanging :as hanging]
            [collider.game.hanging.drops :as drops]
            [collider.game.mode :as game-mode]
            [collider.game.orb :as orb]
            [collider.game.turn.arrow :as arrow]
            [collider.game.turn.falling :as falling]
            [collider.game.turn.landing :as landing]
            [collider.game.turn.thrown :as thrown]
            [collider.game.turn.tnt :as tnt]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.fall :as fall]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.phys :as phys])
  (:import (collider.world Collision)))

(set! *warn-on-reflection* true)

(def ^:private aloof
  #{:area-effect-cloud :marker :interaction :block-display
    :item-display :text-display :ominous-item-spawner})

(def ^:private least (double (float 1.0E-5)))

(defn- axis ^long [d] (case (dir/axis d) :x 0 :y 1 :z 2))

(defn- ahead? [d] (contains? #{:east :up :south} d))

(defn- movement [e]
  (if (:extending? e) (:facing e) (dir/opposite (:facing e))))

(defn- related
  "Returns the state whose shape pushes for the moving piston e, the
  head when it is a base that pulls its head back."
  ^long [e]
  (let [st (long (:moved e))
        sticky? (= :sticky-piston (block/block-of st))
        short? (pos? (long (:progress e)))]
    (if (and (not (:extending? e)) (:source? e)
             (= :piston-base (block/type-of st)))
      (block/state :piston-head {:facing (block/facing-of st)
                                 :short (if short? :true :false)
                                 :type (if sticky? :sticky :normal)})
      st)))

(defn- shape-boxes
  "Returns the boxes of state st at pos as moving piston e holds it."
  [^long st pos e]
  (let [n (long (:progress e))
        k (* 0.5 (if (:extending? e) (- n 2) (- 2 n)))
        o (mapv #(+ (double %1) (* k (double %2)))
                pos (dir/offset (:facing e)))
        at (fn [b] (mapv + b (into o o)))]
    (mapv at (partition 6 (Collision/boxes (block/collision-arr)
                                           (int st))))))

(defn- bounds [bs]
  (vec (concat (apply map min (map #(subvec % 0 3) bs))
               (apply map max (map #(subvec % 3 6) bs)))))

(defn- area
  "Returns the room box b sweeps going dp toward d."
  [b d ^double dp]
  (let [i (axis d)
        base (double (b (if (ahead? d) (+ i 3) i)))
        delta (if (ahead? d) dp (- dp))]
    (assoc b
           i (+ base (min delta 0.0))
           (+ i 3) (+ base (max delta 0.0)))))

(defn- joined [a b]
  (into (mapv min (subvec a 0 3) (subvec b 0 3))
        (mapv max (subvec a 3 6) (subvec b 3 6))))

(defn- meets? [a b]
  (every? #(and (< (double (a %)) (double (b (+ % 3))))
                (> (double (a (+ % 3))) (double (b %))))
          [0 1 2]))

(defn- reach
  "Returns how far box b must go toward d to leave box a."
  ^double [a d b]
  (let [i (axis d)]
    (if (ahead? d)
      (- (double (a (+ i 3))) (double (b i)))
      (- (double (b (+ i 3))) (double (a i))))))

(defn- limited
  "Returns what is left of a push of amount along axis i in tick t
  once the pushes of that tick add up to no more than 0.51, and the
  sums of the pushes so far."
  [e ^long t ^long i ^double amount]
  (let [[at & sums] (:piston-deltas e)
        sums (if (= t at) (vec sums) [0.0 0.0 0.0])
        was (double (sums i))
        now (max -0.51 (min 0.51 (+ amount was)))
        left (- now was)]
    [(if (<= (Math/abs left) least) 0.0 left)
     (into [t] (assoc sums i now))]))

(defn- step-up ^double [e]
  (if (entity/living? e)
    (double (float (:step-height (attribute/base-values e))))
    0.0))

(defn- support [chunks e mv half ctx]
  (let [p (phys/pos mv) o (:pos e)
        sb (phys/supporting-block chunks p half ctx)]
    (if (or sb (:no-blocks? e))
      [sb (nil? sb)]
      (let [s (phys/supporting-block
                chunks (v/v3 (v/x o) (v/y p) (v/z o)) half ctx)]
        [s (nil? s)]))))

(defn- fall-after ^double [e dy]
  (cond-> (double (or (:fall e) 0.0))
    (and (< (double dy) 0.0) (not (:wet? e))) (phys/fallen dy)))

(defn- block-fell [w id e pos f]
  (let [[b ds] (falling/fell-on w id (assoc e :pos pos) f)]
    [(if b (assoc e :block (:block b)) (assoc e :cancel-drop true))
     ds]))

(defn- honey? [chunks pos sup]
  (= :honey (block/type-of (chunk/at chunks
                                     (fall/on-pos chunks pos sup)))))

(defn- fell
  "Returns [e deltas], body e that a push landed after fall f on the
  block under pos with what that block does with the fall."
  [w id e pos sup f]
  (cond
    (not (pos? (double f))) [e nil]
    (entity/living? e)
    (let [[p ds] (landing/landed w id e pos sup (:fall e) f)]
      [(assoc e :pos p) ds])
    (= :falling-block (:type e))
    (block-fell w id e pos f)
    (honey? (:chunks w) pos sup) [e (falling/honey-deltas id e)]
    :else [e nil]))

(defn- speed-factor ^double [chunks e p sup]
  (if (#{:item :experience-orb} (:type e))
    (motion/block-speed-factor chunks p sup motion/drop-offset)
    (motion/block-speed-factor chunks p sup)))

(def ^:private ^:const drag (double (float 0.98)))

(def ^:private living-drag (num/fsub 1.0 (num/fsub 1.0 drag)))

(defn- unliving-fall [t]
  (or (case t
        :tnt [tnt/gravity tnt/drag]
        :falling-block [falling/gravity falling/air-drag]
        :item [0.04 drag]
        :experience-orb [orb/gravity drag]
        nil)
      (when (contains? entity/thrown-types t)
        [(thrown/gravity-of t) thrown/air-drag])
      (throw (ex-info "a body of a kind whose fall is unknown"
                      {:type t}))))

(defn- gravity-drag
  "Returns [gravity air-drag] of body e falling at speed uy."
  [e ^double uy]
  (cond
    (entity/living? e)
    (let [g (double (:gravity (attribute/base-values e)))]
      [(if (and (<= uy 0.0) (contains? (:effects e) :slow-falling))
         (min g 0.01)
         g)
       living-drag])
    (entity/arrow? e) [arrow/gravity arrow/drag]
    :else (unliving-fall (:type e))))

(defn- hit? [d mv i]
  (let [w (phys/vel mv)]
    (not (== (double (d i)) (double ([(v/x w) (v/y w) (v/z w)] i))))))

(defn- stopped [hit? ^double u] (if hit? (* (- u) 0.0) u))

(defn- vertical
  "Returns the vertical speed of body e moving at uy that a push
  stopped up or down by mv, bounced by the block it lands on."
  [chunks e mv sup uy]
  (let [uy (double uy)
        st (chunk/at chunks (fall/on-pos chunks (phys/pos mv) sup))]
    (or (when (phys/on-ground? mv)
          (let [[g dr] (gravity-drag e uy)]
            (motion/bounce st uy (phys/moved-y mv) g dr
                           (entity/living? e))))
        (* (- 0.0 uy) 0.0))))

(defn- restituted
  "Returns speed u of body e after a piston asked d and mv moved it,
  zeroed or bounced on each axis a block stopped, times the speed
  factor of the blocks under it."
  [chunks e d mv sup u]
  (let [hy (not= (double (d 1)) (phys/moved-y mv))
        sf (speed-factor chunks e (phys/pos mv) sup)]
    [(* sf (stopped (hit? d mv 0) (v/x u)))
     (if hy (vertical chunks e mv sup (v/y u)) (v/y u))
     (* sf (stopped (hit? d mv 2) (v/z u)))]))

(defn- fell-in
  "Returns [fields deltas] of body e with fields m whose push moved it
  by mv, after the fall it lands from."
  [w id e m mv sup]
  (let [f (fall-after e (phys/moved-y mv))
        og (:on-ground m)
        [e' ds] (when og (fell w id (merge e m) (:pos m) sup f))]
    [(cond-> m
       (contains? e :fall) (assoc :fall (if og 0.0 f))
       e' (merge (select-keys e' [:pos :block :cancel-drop])))
     ds]))

(defn- body-moved
  "Returns [fields deltas] of body e that a move mv asked as d
  leaves."
  [w id e d mv half ctx]
  (let [chunks (:chunks w)
        og (phys/on-ground? mv)
        [sup nb?] (if og (support chunks e mv half ctx) [nil false])
        u (if (:stuck e) [0.0 0.0 0.0] (:vel e))
        m (cond-> {:pos (phys/pos mv) :on-ground og
                   :vel (restituted chunks e d mv sup u)}
            (:stuck e) (assoc :stuck nil)
            (contains? e :support)
            (assoc :support sup :no-blocks? nb?))]
    (fell-in w id e m mv sup)))

(defn- player-moved [d mv]
  (cond-> {:pos (phys/pos mv)}
    (not= 0.0 (double (d 1)))
    (assoc :on-ground (phys/on-ground? mv))))

(defn- fallen-arrow [w e [m ds]]
  (if (and (:in-ground? e) (arrow/falls? w (merge e m)))
    [(assoc m :in-ground? false :life 0 :ground-time 0) ds]
    [m ds]))

(defn- body-move [w id e dv]
  (let [[half height] (size/box e)
        ctx (phys/context e)
        mv (phys/move (:chunks w) (:pos e) dv half height (step-up e)
                      (boolean (:on-ground e)) ctx)]
    (cond (entity/player? e) [(player-moved dv mv) nil]
          (entity/arrow? e)
          (fallen-arrow w e (body-moved w id e dv mv half ctx))
          :else (body-moved w id e dv mv half ctx))))

(defn- moved
  "Returns [fields deltas] of body id, e, after a piston pushes it by
  amount along d."
  [w id e d amount]
  (let [i (axis d)
        a (if (ahead? d) amount (- (double amount)))
        [left sums] (limited e (:tick w) i a)
        [m ds] (when-not (zero? (double left))
                 (body-move w id e (assoc [0.0 0.0 0.0] i left)))]
    [(assoc m :piston-deltas sums) ds]))

(defn- pushed-by
  "Returns how far boxes going toward d push box b, at most dp."
  ^double [areas d b ^double dp]
  (reduce (fn [^double best a]
            (if (meets? a b)
              (let [r (max best (reach a d b))]
                (if (>= r dp) (reduced r) r))
              best))
          0.0 areas))

(defn- intersection [a b]
  (into (mapv max (subvec a 0 3) (subvec b 0 3))
        (mapv min (subvec a 3 6) (subvec b 3 6))))

(defn- kept-out
  "Returns [fields deltas] of body id, e, after the base at pos, whose
  head goes back toward d, puts it out of its block."
  [w pos id e d dp]
  (let [base (into (mapv double pos) (map #(+ 1.0 (double %)) pos))
        b (bodies/box-of e) o (dir/opposite d)]
    (when (meets? b base)
      (let [r (+ 0.01 (reach base o b))
            ri (+ 0.01 (reach base o (intersection b base)))]
        (when (< (Math/abs (- r ri)) 0.01)
          (moved w id e o (+ 0.01 (min r (double dp)))))))))

(defn- thrown [e d]
  (let [i (axis d) u (:vel e)]
    (assoc e :vel (assoc [(v/x u) (v/y u) (v/z u)] i
                         (if (ahead? d) 1.0 -1.0)))))

(defn- broken
  "Returns the deltas of hanging entity id, e, that a push breaks."
  [w id e]
  (drops/kill-deltas w id e nil nil
                     (drops/gone-deltas w id e (:tick w))))

(defn- shoved
  "Returns the deltas of body id, e, that moving piston pe at pos
  pushes through areas toward d."
  [w pos pe areas d slime? [id e]]
  (let [b (if slime? (thrown e d) e)
        r (pushed-by areas d (bodies/box-of b) 0.5)
        merged #(when (seq %) [[:merge-entity id %]])]
    (cond
      (not (pos? r)) (merged (when slime? {:vel (:vel b)}))
      (contains? hanging/types (:type e)) (broken w id e)
      :else
      (let [[m ds] (moved w id b d (+ 0.01 (min r 0.5)))
            [fix fds] (when (and (not (:extending? pe)) (:source? pe))
                        (kept-out w pos id (merge b m) d 0.5))]
        (concat (merged (merge (select-keys b [:vel]) m fix))
                ds fds)))))

(defn- in? [box e]
  (and e (not (game-mode/spectator? e)) (not (entity/removed? e))
       (not (aloof (:type e))) (meets? (bodies/box-of e) box)))

(defn- in-box [w box]
  (let [es (:entities w)
        [x0 y0 z0 x1 y1 z1] box
        ids (sections/within (sections/index w) (v/v3 x0 y0 z0)
                             (v/v3 x1 y1 z1))]
    (for [id ids :let [e (get es id)] :when (in? box e)] [id e])))

(defn pushes
  "Returns the deltas of the bodies moving piston e at pos pushes in
  its tick."
  [w pos e]
  (let [bs (shape-boxes (related e) pos e)]
    (when (seq bs)
      (let [d (movement e)
            whole (bounds bs)
            areas (mapv #(area % d 0.5) bs)
            slime? (= :slime-block (block/block-of (:moved e)))]
        (mapcat #(shoved w pos e areas d slime? %)
                (remove #(and slime? (entity/player? (% 1)))
                        (in-box w (joined (area whole d 0.5)
                                          whole))))))))
