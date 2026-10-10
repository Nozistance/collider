(ns collider.game.commands.pos
  "The positions and turns commands take, and the checks on them."
  (:require [collider.game.areas :as areas]
            [collider.game.command.targets :as targets]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(defn unloaded?
  "Returns true when one of the positions is in no loaded chunk."
  [world positions]
  (let [held (areas/needed-ids world)]
    (some #(let [id (chunk/block-chunk %)]
             (not (or (contains? (:chunks world) id)
                      (contains? held id))))
          positions)))

(defn- flat-in? [^long x ^long z]
  (and (<= -30000000 x) (< x 30000000)
       (<= -30000000 z) (< z 30000000)))

(defn- in-world?
  [world [x y z]]
  (and (chunk/in-level? world (long y))
       (flat-in? (long x) (long z))))

(defn pos-error
  "Returns the error key of block position pos, nil when it is fine."
  [world pos]
  (cond
    (unloaded? world [pos]) "argument.pos.unloaded"
    (not (in-world? world pos)) "argument.pos.outofworld"))

(defn spawnable?
  "Returns true when an entity may stand at pos."
  [pos]
  (let [[x y z] (mapv #(long (Math/floor (double %))) pos)]
    (and (<= -20000000 (long y)) (< (long y) 20000000)
         (flat-in? x z))))

(defn out-of-bounds?
  "Returns true when pos is given and no entity may stand there."
  [pos]
  (and (every? some? pos) (not (spawnable? pos))))

(defn block-under
  "Returns the block of pos, the source block where pos gives none."
  [world [x y z]]
  (let [p (targets/source-pos world)
        at #(long (Math/floor (double (nth p %))))]
    [(long (or x (at 0)))
     (long (or y (at 1)))
     (long (or z (at 2)))]))

(defn turn-of
  "Returns the yaw and pitch that player eid gives as arguments."
  [world eid yaw pitch]
  (let [e (get-in world [:entities eid])
        get (fn [[rel? v] own]
              (let [base (if rel? (double (or own 0.0)) 0.0)]
                (float (+ (double v) base))))
        y (if yaw (get yaw (:yaw e)) (float 0.0))
        p (if pitch (get pitch (:pitch e)) (float 0.0))]
    [(targets/wrapped y) (float (max -90.0 (min 90.0 (double p))))]))
