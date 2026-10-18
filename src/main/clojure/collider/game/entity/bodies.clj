(ns collider.game.entity.bodies
  "The bodies whose boxes meet a box, as block triggers count them."
  (:require [collider.data :as data]
            [collider.game.entity :as entity]
            [collider.game.entity.sections :as sections]
            [collider.game.entity.size :as size]
            [collider.game.hanging :as hanging]
            [collider.game.mode :as game-mode]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- box-of
  "Returns the box [x0 y0 z0 x1 y1 z1] of body e."
  [e]
  (if (contains? hanging/types (:type e))
    (hanging/box e)
    (let [[half height] (or (size/box e)
                            (throw (ex-info "a body of no size"
                                            {:type (:type e)})))
          p (:pos e) half (double half)
          x (double (v/x p)) y (double (v/y p)) z (double (v/z p))]
      [(- x half) y (- z half) (+ x half) (+ y (double height))
       (+ z half)])))

(defn- meets? [[x0 y0 z0 x1 y1 z1] e]
  (let [[a0 a1 a2 a3 a4 a5] (box-of e)]
    (and (< (double a0) (double x1)) (> (double a3) (double x0))
         (< (double a1) (double y1)) (> (double a4) (double y0))
         (< (double a2) (double z1)) (> (double a5) (double z0)))))

(defn ignores-triggers?
  "Returns true when body e neither presses nor weighs on a block."
  [e]
  (let [x (:ignores-triggers? (get (data/entities) (:type e)))]
    (if (= :varies x)
      (throw (ex-info "a body whose state decides if it ignores blocks"
                      {:type (:type e)}))
      (boolean x))))

(defn- counts? [box living? e]
  (and (some? e)
       (or (not living?) (entity/living? e))
       (not (ignores-triggers? e))
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
