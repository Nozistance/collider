(ns collider.game.systems.presses
  "Bodies pressing the plates they are in once they have moved."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.changes :as changes]
            [collider.game.deltas :as deltas]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.plate :as plate]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:table plates
  (delay (block/state-table :boolean plate/plate?)))

(def ^:private unmoved
  #{:player :painting :item-frame :glow-item-frame :leash-knot
    :lightning-bolt :area-effect-cloud})

(defn- up? [^long st]
  (and (plate/plate? st) (zero? (plate/signal-of st))))

(defn- sec ^long [^double a] (bit-shift-right (num/floor a) 4))

(defn- holds-at? [chunks ^long cx ^long sy ^long cz]
  (if-let [s (chunk/chunk-section (chunk/chunk-at chunks cx cz)
                                  (chunk/section-index (* 16 sy)))]
    (chunk/holds? s @plates)
    false))

(defn- plated?
  "Returns true when a section the box of a body at p meets may hold
  a plate."
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
  "Returns the cells of the plates at rest that the box of body e is
  in, in the order it meets them. A dying body meets none."
  [chunks e]
  (let [[half height] (or (size/box e)
                          (throw (ex-info "a body of no size"
                                          {:type (:type e)})))
        p (:pos e)]
    (when (and (not (dying? e)) (plated? chunks p half height))
      (for [[x y z st] (inside/met chunks [[p p nil]] half height
                                   @plates)
            :when (up? st)]
        [x y z]))))

(defn- pressed [[w :as acc] p]
  (let [st (chunk/at (:chunks w) p)]
    (if (up? st)
      (let [t (:tick w)]
        (apply/then acc (changes/change-deltas
                          w (plate/checked p st (bodies/counter w) t) t)))
      acc)))

(defn- presses-of [world cells]
  (second (reduce pressed [world deltas/empty-deltas] (distinct cells))))

(defn presses
  "Returns the deltas of the plates the bodies but the players press
  after their turns, body by body."
  {:wake {:keys [:entities]}}
  [world _d]
  (let [chunks (:chunks world) active (areas/active-chunks world)]
    (presses-of world
                (for [[_ e] (:entities world)
                      :when (and (not (unmoved (:type e)))
                                 (areas/active-at? active (:pos e)))
                      c (touched chunks e)]
                  c))))

(defn player-deltas
  "Returns the deltas of the plates player e presses in its tick."
  [world [_ e]]
  (when-not (game-mode/spectator? e)
    (deltas/as-vec (presses-of world (touched (:chunks world) e)))))
