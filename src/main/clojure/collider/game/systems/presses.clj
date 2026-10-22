(ns collider.game.systems.presses
  "Bodies stepping on blocks and pressing the plates and string they
  are in once they have moved."
  (:require [collider.game.apply :as apply]
            [collider.game.areas :as areas]
            [collider.game.block.press :as press]
            [collider.game.block.step :as step]
            [collider.game.deltas :as deltas]
            [collider.game.entity.bodies :as bodies]
            [collider.game.entity.size :as size]
            [collider.game.input :as input]
            [collider.game.mode :as game-mode]
            [collider.math :as math]
            [collider.vec :as v]
            [collider.world.blocks.inside :as inside]
            [collider.world.blocks.plate :as plate]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private unmoved
  #{:player :painting :item-frame :glow-item-frame :leash-knot
    :lightning-bolt :area-effect-cloud})

(defn- sec ^long [^double a] (bit-shift-right (math/floor a) 4))

(defn- holds-at? [chunks ^long cx ^long sy ^long cz]
  (if-let [s (chunk/chunk-section (chunk/chunk-at chunks cx cz)
                                  (chunk/section-index (* 16 sy)))]
    (chunk/holds? s @press/pressables)
    false))

(defn- pressable-near?
  "Returns true when a section the box of a body on moves meets may
  hold a plate or string."
  [chunks moves ^double half ^double height]
  (let [ps (mapcat #(subvec (vec %) 0 2) moves)
        lo #(apply min (map (fn [p] (double (% p))) ps))
        hi #(apply max (map (fn [p] (double (% p))) ps))
        y0 (sec (lo v/y)) y1 (sec (+ (hi v/y) height))
        z0 (sec (- (lo v/z) half)) z1 (sec (+ (hi v/z) half))
        x1 (sec (+ (hi v/x) half))
        column? (fn [cx cz]
                  (loop [sy y0]
                    (and (<= sy y1)
                         (or (holds-at? chunks cx sy cz)
                             (recur (inc sy))))))]
    (loop [cx (sec (- (lo v/x) half)) cz z0]
      (cond (> cx x1) false
            (> cz z1) (recur (inc cx) z0)
            (column? cx cz) true
            :else (recur cx (inc cz))))))

(defn- dying? [e]
  (when-some [h (:health e)] (<= (double h) 0.0)))

(defn- touched
  "Returns the cells of the plates and string at rest that the box of
  body e meets on moves, in the order it meets them. A dying body
  meets none, one that ignores triggers no string."
  [world e moves]
  (let [chunks (:chunks world)
        [half height] (or (size/box e)
                          (throw (ex-info "a body of no size"
                                          {:type (:type e)})))]
    (when (and (not (dying? e))
               (pressable-near? chunks moves half height))
      (for [[x y z st] (inside/met chunks moves half height
                                   @press/pressables)
            :when (and (press/up? world [x y z] st)
                       (or (plate/plate? st)
                           (not (bodies/ignores-triggers? e))))]
        [x y z]))))

(defn- bodies-of
  "Returns [eid body moves] of the bodies that press after their
  turns in the order of their ids, along the moves their turns queued,
  those gone since too, or where they stand."
  [world]
  (let [active (areas/active-chunks world)
        es (:entities world) trips (:trips world)
        live (for [[eid e] es
                   :when (and (not (unmoved (:type e)))
                              (areas/active-at? active (:pos e)))
                   :let [p (:pos e)]]
               [eid e (:moves (get trips eid) [[p p nil]])])
        gone (for [[eid t] trips :when (not (contains? es eid))]
               [eid t (:moves t)])]
    (if (seq gone) (sort-by first (concat live gone)) live)))

(defn- affected
  "Returns acc after body e of eid steps on the block under it, when
  it is still there, and presses what it met on moves."
  [[w :as acc] [eid e moves]]
  (-> (if (contains? (:entities w) eid)
        (apply/then acc (step/deltas w eid e))
        acc)
      (press/pressed-in (touched w e moves))))

(defn presses
  "Returns the deltas of the blocks the bodies but the players step on
  and press after their turns, body by body."
  {:wake {:keys [:entities :trips]}}
  [world _d]
  (cond-> (second (reduce affected [world deltas/empty-deltas]
                          (bodies-of world)))
    (:trips world) (deltas/merge (deltas/of-vec [[:trips-pressed]]))))

(defn player-deltas
  "Returns the deltas of the block player e of eid steps on and the
  plates and string it presses in its tick, along the moves
  its packets made."
  [world [eid e]]
  (when-not (game-mode/spectator? e)
    (let [p (:pos e)
          moves (or (seq (input/moves-of world eid)) [[p p nil]])]
      (deltas/as-vec
        (second (affected [world deltas/empty-deltas] [eid e moves]))))))
