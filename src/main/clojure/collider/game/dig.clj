(ns collider.game.dig
  "How fast a player digs a block."
  (:require [collider.data :as data]
            [collider.game.attribute :as attribute]
            [collider.game.player :as player]
            [collider.game.tool :as tool]
            [collider.math :as math]
            [collider.world.block :as block]))

(set! *warn-on-reflection* true)

(defn- attr ^double [e k]
  (math/f32 (attribute/value e (:effects e) k)))

(defn- amplifier [e k]
  (some-> (get-in e [:effects k :amplifier]) long))

(defn- hasted ^double [e ^double speed]
  (let [a (amplifier e :haste) b (amplifier e :conduit-power)]
    (if (or a b)
      (let [amp (max (long (or a 0)) (long (or b 0)))
            boost (math/f32 (+ 1.0 (math/f32 (* (inc amp) 0.2))))]
        (math/f32 (* speed boost)))
      speed)))

(def ^:private fatigue-scales
  [(math/f32 0.3) (math/f32 0.09) (math/f32 0.0027)])

(defn- fatigued ^double [e ^double speed]
  (if-let [a (amplifier e :mining-fatigue)]
    (math/f32 (* speed (double (get fatigue-scales a (math/f32 8.1E-4)))))
    speed))

(defn- base-speed ^double [e stack st]
  (let [s (math/f32 (tool/mining-speed stack st))]
    (if (> s 1.0) (math/f32 (+ s (attr e :mining-efficiency))) s)))

(defn destroy-speed
  "Returns how fast player e digs st with the stack it holds."
  ^double [e st]
  (let [s (->> (base-speed e (player/hand-stack e :main) st)
               (hasted e) (fatigued e))
        s (math/f32 (* s (attr e :block-break-speed)))
        s (if (:eye-in-water? e)
            (math/f32 (* s (attr e :submerged-mining-speed)))
            s)]
    (if (:on-ground e) s (math/f32 (/ s 5.0)))))

(defn correct-tool?
  "Returns true when player e holds what makes st drop."
  [e st]
  (or (not (:needs-tool? (get (data/blocks) (block/block-of st))))
      (tool/correct-for-drops? (player/hand-stack e :main) st)))

(defn progress
  "Returns the part of st that player e digs away in one tick."
  ^double [e st]
  (let [b (get (data/blocks) (block/block-of st))
        h (math/f32 (:hardness b 0.0))]
    (if (== h -1.0)
      0.0
      (let [div (if (correct-tool? e st) 30.0 100.0)]
        (math/f32 (/ (math/f32 (/ (destroy-speed e st) h)) div))))))
