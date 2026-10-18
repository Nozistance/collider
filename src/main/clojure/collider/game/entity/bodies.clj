(ns collider.game.entity.bodies
  "The bodies whose boxes meet a box, as block triggers count them."
  (:require [collider.game.entity :as entity]
            [collider.game.entity.sections :as sections]
            [collider.game.entity.size :as size]
            [collider.game.mode :as game-mode]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- meets? [[x0 y0 z0 x1 y1 z1] e]
  (let [[half height] (or (size/box e)
                          (throw (ex-info "a body of no size"
                                          {:type (:type e)})))
        p (:pos e) half (double half)
        x (double (v/x p)) y (double (v/y p)) z (double (v/z p))]
    (and (< (- x half) (double x1)) (> (+ x half) (double x0))
         (< y (double y1)) (> (+ y (double height)) (double y0))
         (< (- z half) (double z1)) (> (+ z half) (double z0)))))

(defn- counts? [box living? e]
  (and (some? e)
       (or (not living?) (entity/living? e))
       (not (game-mode/spectator? e))
       (not (entity/removed? e))
       (meets? box e)))

(defn counter
  "Returns the fn of a box and living? that counts the bodies of level
  lv whose boxes meet the box, only players and mobs when living?.
  Spectators and players gone from the level do not count."
  [lv]
  (fn [[x0 y0 z0 x1 y1 z1 :as box] living?]
    (let [es (:entities lv)]
      (count (filter #(counts? box living? (get es %))
                     (sections/within (sections/index lv)
                                      (v/v3 x0 y0 z0)
                                      (v/v3 x1 y1 z1)))))))
