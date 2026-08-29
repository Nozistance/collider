(ns collider.game.systems.blocks
  (:require [collider.rnd :as rnd]
            [collider.game.mobs :as mobs]
            [collider.game.state :as state]
            [collider.game.tnt :as tnt]
            [collider.proto.packets.play :as play]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.orientation :as orient]
            [collider.world.fire :as fire]
            [collider.world.liquid :as liquid]))

(set! *warn-on-reflection* true)

(def ^:private dig-sound
  (merge
   (zipmap [5 17 25 47 53 54 58 63 65 72 85 96 107 126 143 162] (repeat "dig.wood"))
   (zipmap [2 18 31 46 106 161] (repeat "dig.grass"))
   (zipmap [3 13 60] (repeat "dig.gravel"))
   {12 "dig.sand", 19 "dig.cloth", 35 "dig.cloth", 171 "dig.cloth"
    20 "dig.glass", 79 "dig.glass", 95 "dig.glass", 102 "dig.glass"
    78 "dig.snow", 80 "dig.snow"}))

(defn- block-at ^long [world pos]
  (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos))

(defn- placed-state [world eid item-id damage face cursor]
  (bit-or (bit-shift-left (long item-id) 4)
          (orient/placement-meta item-id damage face
                          (get-in world [:entities eid :yaw] 0.0)
                          (nth cursor 1))))

(defn- intersects-player? [world [x y z]]
  (let [x (double x) y (double y) z (double z)]
    (some (fn [[_ e]]
            (when (= :player (:type e))
              (let [[px py pz] (:pos e)
                    px (double px) py (double py) pz (double pz)]
                (and (> (+ px 0.3) x) (< (- px 0.3) (+ x 1.0))
                     (> (+ py 1.8) y) (< py (+ y 1.0))
                     (> (+ pz 0.3) z) (< (- pz 0.3) (+ z 1.0))))))
          (:entities world))))

(defn- reject-deltas [world eid pos pos']
  (concat
   [[:send eid (play/block-change pos (block-at world pos))]]
   (when pos'
     [[:send eid (play/block-change pos' (block-at world pos'))]])))

(defn- placed-deltas [world pos state item-id]
  (concat
   [[:set-block pos state]]
   (state/broadcast world (play/block-change pos state))
   (state/broadcast world (play/named-sound (get dig-sound (long item-id) "dig.stone")
                                            pos 1.0 50))))

(defn- extinguish-deltas [world [_ status pos face]]
  (when (= 0 status)
    (when-let [off (orient/face-offsets face)]
      (let [[_ y' _ :as pos'] (mapv + pos off)]
        (when (and (<= 0 (long y') 255)
                   (fire/fire-state? (block-at world pos')))
          (concat
           [[:set-block pos' 0]]
           (state/broadcast world (play/block-change pos' 0))
           (state/broadcast world (play/fizz-effect pos'))))))))

(defn- dig-deltas [world [eid status pos _face :as args]]
  (or (extinguish-deltas world args)
      (let [old (block-at world pos)]
        (when (and (or (= 0 status) (= 2 status)) (pos? old))
          (concat
           [[:set-block pos 0]]
           (state/broadcast world (play/block-change pos 0))
           (state/broadcast world eid (play/break-effect pos old)))))))

(defn- occupied? [world pos]
  (let [cur (block-at world pos)]
    (and (pos? cur)
         (not (liquid/liquid-state? cur))
         (not (fire/fire-state? cur)))))

(def ^:private fire-state (bit-shift-left 51 4))
(defn- flint-deltas [world [eid pos face]]
  (when-let [off (orient/face-offsets face)]
    (if (and (tnt/tnt-state? (block-at world pos))
             (not (get-in world [:entities eid :sneaking?]))
             (not ((tnt/primed-origins world) pos)))
      (concat
       [[:spawn-entity (tnt/primed pos [(:tick world) pos])]]
       (state/broadcast world (play/named-sound "game.tnt.primed" pos 1.0 63)))
      (let [[_ y' _ :as pos'] (mapv + pos off)]
        (when (and (<= 0 (long y') 255) (zero? (block-at world pos')))
          (concat
           [[:set-block pos' fire-state]]
           (state/broadcast world (play/block-change pos' fire-state))
           (state/broadcast world (play/named-sound "fire.ignite" pos' 1.0 63))))))))

(defn- solid-place-deltas [world [eid pos face item-id damage cursor]]
  (when-let [off (orient/face-offsets face)]
    (when (<= 1 (long item-id) 255)
      (let [[_ y' _ :as target] (mapv + pos off)
            pos'   (when (<= 0 (long y') 255) target)
            merged (orient/slab-merge (:chunks world) pos pos' face item-id damage)]
        (cond
          merged
          (let [[mp ms] merged]
            (if (intersects-player? world mp)
              (reject-deltas world eid pos pos')
              (placed-deltas world mp ms item-id)))
          (nil? pos') nil
          (occupied? world pos')
          (reject-deltas world eid pos pos')
          (and (not (orient/no-collision (long item-id)))
               (intersects-player? world pos'))
          (reject-deltas world eid pos pos')
          :else
          (placed-deltas world pos'
                         (placed-state world eid item-id damage face cursor)
                         item-id))))))

(defn- look-dir [e]
  (let [yaw   (Math/toRadians (double (:yaw e)))
        pitch (Math/toRadians (double (:pitch e)))]
    [(- (* (Math/sin yaw) (Math/cos pitch)))
     (- (Math/sin pitch))
     (* (Math/cos yaw) (Math/cos pitch))]))

(defn- ray-cell [[ex ey ez] [dx dy dz] t]
  [(long (Math/floor (+ (double ex) (* (double dx) (double t)))))
   (long (Math/floor (+ (double ey) (* (double dy) (double t)))))
   (long (Math/floor (+ (double ez) (* (double dz) (double t)))))])

(defn- pour-target [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [[px py pz] (:pos e)
          eye [(double px) (+ (double py) 1.62) (double pz)]
          dir (look-dir e)]
      (loop [t 0.0 prev nil]
        (when (<= t 5.0)
          (let [[_ by _ :as pos] (ray-cell eye dir t)
                st (if (<= 0 (long by) 255) (block-at world pos) 0)]
            (if (and (pos? st) (not (liquid/liquid-state? st)))
              prev
              (recur (+ t 0.1) pos))))))))

(defn- bucket-deltas [world [eid _ _] state]
  (when-let [[_ y' _ :as pos'] (pour-target world eid)]
    (when (<= 0 (long y') 255)
      (let [cur (block-at world pos')]
        (when (or (zero? cur) (liquid/liquid-state? cur))
          (cons [:set-block pos' state]
                (state/broadcast world (play/block-change pos' state))))))))

(defn- scoop-target [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [[px py pz] (:pos e)
          eye [(double px) (+ (double py) 1.62) (double pz)]
          dir (look-dir e)]
      (loop [t 0.0]
        (when (<= t 5.0)
          (let [[_ by _ :as pos] (ray-cell eye dir t)
                st (if (<= 0 (long by) 255) (block-at world pos) 0)]
            (cond
              (liquid/source-state? st) pos
              (liquid/liquid-state? st) (recur (+ t 0.1))
              (pos? st) nil
              :else (recur (+ t 0.1)))))))))

(defn- scoop-deltas [world eid]
  (when-let [pos (scoop-target world eid)]
    (cons [:set-block pos 0]
          (state/broadcast world (play/block-change pos 0)))))

(def ^:private armor-ids
  (into #{} (range 298 318)))

(defn- equip-armor-deltas [world eid item-id damage]
  (let [e    (get-in world [:entities eid])
        slot (+ 5 (mod (- (long item-id) 298) 4))]
    (when (and e (nil? (get-in e [:inventory slot])))
      (let [held  (+ 36 (long (or (:held-slot e) 0)))
            stack (or (get-in e [:inventory held])
                      {:item (long item-id) :count 1 :damage (long damage)})]
        [[:set-slot eid slot stack]
         [:set-slot eid held nil]]))))

(defn- spawn-egg-deltas [world [_ pos face _ damage]]
  (when-let [off (orient/face-offsets face)]
    (when-let [mob (mobs/egg-type damage)]
      (let [[x y z] (mapv + pos off)
            t (:tick world)
            at [(+ (long x) 0.5) (double y) (+ (long z) 0.5)]
            pitch (long (* 63.0 (+ 1.0 (* 0.2 (- (rnd/rnd [t pos :p1])
                                                 (rnd/rnd [t pos :p2]))))))]
        (when (<= 0 (long y) 255)
          (cons [:spawn-entity (mobs/egg-mob mob at [t pos] t)]
                (when-let [say (mobs/say-sound mob)]
                  (state/broadcast world (play/entity-sound say at 1.0 pitch)))))))))

(defn- place-deltas [world [eid _ face item-id damage :as args]]
  (let [use-item? (= 255 (bit-and (long face) 0xFF))
        pour      (liquid/bucket->state item-id)]
    (cond
      pour                     (when use-item? (bucket-deltas world args pour))
      (= 259 (long item-id))   (when-not use-item? (flint-deltas world args))
      (= 325 (long item-id))   (when use-item? (scoop-deltas world eid))
      (= 383 (long item-id))   (when-not use-item? (spawn-egg-deltas world args))
      (armor-ids (long item-id)) (when use-item?
                                   (equip-armor-deltas world eid item-id damage))
      :else                    (solid-place-deltas world args))))

(defn- block-edits-deltas [world events]
  (into []
        (mapcat (fn [[tag & args]]
                  (case tag
                    :dig   (dig-deltas world args)
                    :place (place-deltas world args)
                    nil)))
        events))

(defn block-edits [world events]
  [#(block-edits-deltas world events)])
