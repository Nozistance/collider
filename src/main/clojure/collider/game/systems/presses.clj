(ns collider.game.systems.presses
  "Bodies pressing the plates and string they are in once they have
  moved."
  (:require [collider.game.areas :as areas]
            [collider.game.block.press :as press]
            [collider.game.deltas :as deltas]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.plate :as plate]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private unmoved
  #{:player :painting :item-frame :glow-item-frame :leash-knot
    :lightning-bolt :area-effect-cloud})

(defn- sec ^long [^double a] (bit-shift-right (num/floor a) 4))

(defn- holds-at? [chunks ^long cx ^long sy ^long cz]
  (if-let [s (chunk/chunk-section (chunk/chunk-at chunks cx cz)
                                  (chunk/section-index (* 16 sy)))]
    (chunk/holds? s @press/pressables)
    false))

(defn- plated?
  "Returns true when a section the box of a body at p meets may hold
  a plate or string."
  [chunks p ^double half ^double height]
  (let [x (double (v/x p)) y (double (v/y p)) z (double (v/z p))
        y0 (sec y) y1 (sec (+ y height)) z0 (sec (- z half))
        z1 (sec (+ z half)) x1 (sec (+ x half))
        column? (fn [cx cz]
                  (loop [sy y0]
                    (and (<= sy y1)
                         (or (holds-at? chunks cx sy cz)
                             (recur (inc sy))))))]
    (loop [cx (sec (- x half)) cz z0]
      (cond (> cx x1) false
            (> cz z1) (recur (inc cx) z0)
            (column? cx cz) true
            :else (recur cx (inc cz))))))

(defn- dying? [e]
  (when-some [h (:health e)] (<= (double h) 0.0)))

(defn- touched
  "Returns the cells of the plates and string at rest that the box of
  body e is in, in the order it meets them. A dying body meets none,
  one that ignores triggers no string."
  [world e]
  (let [chunks (:chunks world)
        [half height] (or (size/box e)
                          (throw (ex-info "a body of no size"
                                          {:type (:type e)})))
        p (:pos e)]
    (when (and (not (dying? e)) (plated? chunks p half height))
      (for [[x y z st] (inside/met chunks [[p p nil]] half height
                                   @press/pressables)
            :when (and (press/up? world [x y z] st)
                       (or (plate/plate? st)
                           (not (bodies/ignores-triggers? e))))]
        [x y z]))))

(defn presses
  "Returns the deltas of the plates and string the bodies but the
  players press after their turns, body by body."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [active (areas/active-chunks world)]
    (press/pressed-at world
                (for [[_ e] (:entities world)
                      :when (and (not (unmoved (:type e)))
                                 (areas/active-at? active (:pos e)))
                      c (touched world e)]
                  c))))

(defn player-deltas
  "Returns the deltas of the plates and string player e presses in
  its tick."
  [world [_ e]]
  (when-not (game-mode/spectator? e)
    (deltas/as-vec (press/pressed-at world (touched world e)))))
