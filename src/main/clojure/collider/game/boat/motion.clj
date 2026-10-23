(ns collider.game.boat.motion
  "Movement and paddles sent by the player steering a boat."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.mob.push :as push]
            [collider.game.mode :as mode]
            [collider.game.out :as out]
            [collider.game.player :as player]
            [collider.math :as math]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.fall :as fall]
            [collider.world.blocks.motion :as motion]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]))

(set! *warn-on-reflection* true)

(def ^:private invalid-reason
  {:translate "multiplayer.disconnect.invalid_vehicle_movement"})

(defn- invalid? [{:keys [pos yaw pitch]}]
  (or (some #(Double/isNaN (double %)) pos)
      (not (Float/isFinite (unchecked-float yaw)))
      (not (Float/isFinite (unchecked-float pitch)))))

(defn- controlled [world eid]
  (let [p (get (:entities world) eid)
        bid (:vehicle p)
        b (get (:entities world) bid)]
    (when (and (entity/player? p) (entity/boat? (:type b))
               (= eid (first (:passengers b))))
      [p bid b])))

(defn- clamped [pos]
  (v/v3 (max -3.0E7 (min 3.0E7 (v/x pos)))
        (max -2.0E7 (min 2.0E7 (v/y pos)))
        (max -3.0E7 (min 3.0E7 (v/z pos)))))

(defn- correction [eid b]
  (out/to eid (out/move-vehicle (:pos b) (:yaw b) (:pitch b))))

(defn- difference [a b]
  (v/v3 (- (v/x a) (v/x b)) (- (v/y a) (v/y b))
        (- (v/z a) (v/z b))))

(defn- inflated [box amount]
  (mapv (fn [i]
          (+ (double (nth box i)) (if (< i 3) (- amount) amount)))
        (range 6)))

(defn- swept [box delta]
  (mapv (fn [i]
          (let [d (double (nth delta (mod i 3)))]
            (+ (double (nth box i)) ((if (< i 3) min max) 0.0 d))))
        (range 6)))

(defn- root-of [entities eid]
  (loop [id eid seen #{}]
    (if-let [vehicle (:vehicle (get entities id))]
      (if (contains? seen vehicle) id
          (recur vehicle (conj seen id)))
      id)))

(defn- solid? [world root eid e]
  (and (not (mode/spectator? e)) (not (entity/removed? e))
       (not= root (root-of (:entities world) eid))
       (or (entity/boat? (:type e))
           (and (entity/living? e)
                (push/pushable? (:chunks world) e)))))

(defn- entity-boxes [world bid area]
  (let [area (inflated area 1.0E-7)
        root (root-of (:entities world) bid)]
    (into []
          (keep (fn [[eid e]]
                  (when (and (not= bid eid) (solid? world root eid e))
                    (let [box (bodies/box-of e)]
                      (when (v/boxes-meet? box area) box)))))
          (:entities world))))

(defn- border-boxes [pos box]
  (let [r (double chunk/world-border)
        reach (max 1.0 (- (double (nth box 3)) (double (nth box 0)))
                   (- (double (nth box 5)) (double (nth box 2))))
        x (v/x pos) z (v/z pos)
        gap (min (- r x) (+ r x) (- r z) (+ r z))
        n Double/NEGATIVE_INFINITY p Double/POSITIVE_INFINITY]
    (when (and (< gap (* 2.0 reach))
               (<= (- (- r) reach) x) (< x (+ r reach))
               (<= (- (- r) reach) z) (< z (+ r reach)))
      [[n n n (- r) p p] [r n n p p p]
       [n n n p p (- r)] [n n r p p p]])))

(defn- old-free? [world bid b box]
  (and (phys/box-free? (:chunks world) box (v/y (:pos b))
                      (phys/context b))
       (empty? (entity-boxes world bid box))
       (not-any? #(phys/joined? % box) (border-boxes (:pos b) box))))

(def ^:private inset (double (float 1.0E-5)))

(defn- new-collision? [world bid b before after]
  (let [before (inflated before (- inset))
        after (inflated after (- inset))
        bottom (v/y (:pos b)) ctx (phys/context b)]
    (or (phys/new-collision? (:chunks world) before after
          bottom ctx)
        (some #(not (phys/joined? % before))
              (entity-boxes world bid after)))))

(defn- water-below? [chunks pos]
  (let [[x y z] (v/cell pos)]
    (block/water? (chunk/at chunks [x (dec y) z]))))

(defn- fallen [chunks b pos dy ground]
  (let [fall (double (or (:fall b) 0.0))]
    (cond ground 0.0
          (and (neg? dy) (not (water-below? chunks pos)))
          (phys/fallen fall dy)
          :else fall)))

(defn- supported [chunks b pos moved ground]
  (if-not ground
    {:support nil :no-blocks? false}
    (let [half (first (size/box b)) ctx (phys/context b)
          at #(phys/supporting-block chunks % half ctx)
          from (v/v3 (- (v/x pos) (v/x moved)) (v/y pos)
                     (- (v/z pos) (v/z moved)))
          s (or (at pos) (when-not (:no-blocks? b) (at from)))]
      {:support s :no-blocks? (nil? s)})))

(defn- kept-speed [chunks b at]
  (let [vel (:vel b)
        factor (motion/block-speed-factor chunks at (:support b))]
    (v/v3 (* (v/x vel) factor) (v/y vel) (* (v/z vel) factor))))

(defn- prior-fall [chunks b pos]
  (let [f (double (or (:fall b) 0.0))]
    (if (and (not (zero? f)) (>= (v/dist-sq (:pos b) pos) 1.0)
             (fall/resets? chunks (:pos b) pos))
      0.0 f)))

(defn- moved-state [world b delta mv]
  (let [chunks (:chunks world) pos (phys/pos mv)
        moved (difference pos (:pos b))
        b (assoc b :fall (prior-fall chunks b pos))
        ground (boolean (phys/on-ground? mv))
        m (cond-> {:fall (:fall b)}
            (not (zero? (v/y delta)))
            (merge (supported chunks b pos moved ground)
                   {:on-ground ground}))]
    (assoc m :vel (kept-speed chunks (merge b m) pos))))

(defn- trial [world bid b delta]
  (let [area (swept (bodies/box-of b) delta)
        extra (into (entity-boxes world bid area)
                    (border-boxes (:pos b) area))
        [half height] (size/box b)]
    (phys/move-among (:chunks world) (:pos b) delta half height
                     (phys/context b) extra)))

(defn- rejected? [world bid b target mv]
  (let [before (bodies/box-of b)
        after (bodies/box-of (assoc b :pos target))
        failed? (> (v/dist-xz-sq target (phys/pos mv)) 0.0625)]
    (or (and failed? (old-free? world bid b before))
        (new-collision? world bid b before after))))

(defn- turned [changes]
  (let [pitch (math/wrap-degrees (:pitch changes))]
    {:yaw (math/wrap-degrees (:yaw changes))
     :pitch (max -90.0 (min 90.0 pitch))}))

(defn- accepted [world eid bid b target changes state]
  (let [ground (boolean (:on-ground changes))
        moved (difference target (:pos b))
        b (merge b state)
        fall (fallen (:chunks world) b target (v/y moved) ground)
        m (merge state (turned changes)
                 (supported (:chunks world) b target moved ground)
                 {:pos target :last-yd (v/y (:vel state))
                  :on-ground ground :fall fall})
        driver {:vehicle-last-good target
                :client-vel moved :client-moved? true}]
    [[:merge-entity bid m] [:merge-entity eid driver]]))

(defn- attempted [world eid p bid b target changes]
  (let [delta (difference target (:vehicle-last-good p))
        mv (trial world bid b delta)
        state (moved-state world b delta mv)]
    (if (rejected? world bid (merge b state) target mv)
      [[:merge-entity bid (merge state (turned changes))]
       (correction eid (merge b (turned changes)))]
      (accepted world eid bid b target changes state))))

(defn- ready? [world p]
  (and (not (:tp-target p)) (player/client-loaded? p (:tick world))
       (:vehicle-first-good p) (:vehicle-last-good p)))

(defn- steered [world eid p bid b changes]
  (let [target (clamped (:pos changes))
        moved (v/dist-sq target (:vehicle-first-good p))
        expected (v/dot (:vel b) (:vel b))]
    (if (> (- moved expected) 100.0)
      [(correction eid b)]
      (attempted world eid p bid b target changes))))

(defn move-deltas [world [_ eid changes]]
  (if (invalid? changes)
    [(out/to eid (out/disconnect invalid-reason))
     (out/to eid (out/close))]
    (when-let [[p bid b] (controlled world eid)]
      (when (ready? world p) (steered world eid p bid b changes)))))

(defn paddle-deltas [world [_ eid changes]]
  (when-let [[_ bid _] (controlled world eid)]
    (let [paddles
          (select-keys changes [:left-paddle? :right-paddle?])]
      [[:merge-entity bid paddles]])))
