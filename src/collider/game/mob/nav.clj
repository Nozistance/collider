(ns collider.game.mob.nav
  "Ground navigation of mobs."
  (:require [collider.game.entity :as entity]
            [collider.game.mob.control :as control]
            [collider.game.mob.mobs :as mobs]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.phys :as phys]
            [collider.world.space.path :as path])
  (:import (collider.game.mob Nav)))

(set! *warn-on-reflection* true)

(def ^:private ^:const follow-range 16.0)

(def ^:private ^:const recompute-gap 20)

(defn- nav-of [e] (or (:nav e) Nav/FRESH))

(defn- half-of ^double [e] (double (nth (mobs/box-of e) 0)))

(defn- path-length
  "PathNavigation.getMaxPathLength: the follow range of mob e with the
  bonus Mob.finalizeSpawn drew, never under the 16 a path needs."
  ^double [e]
  (let [b (double (or (:follow-bonus e) 0.0))
        v (float (+ follow-range (* follow-range b)))]
    (double (Math/max v (float follow-range)))))

(defn- walker [e]
  (let [[half height] (mobs/box-of e)
        len (float (path-length e))
        visits (long (Math/floor (* len (float 16.0))))]
    (assoc (mobs/walker (:type e))
      :max-visited visits :max-up-step (mobs/step-height (:type e))
      :width (* 2.0 (double half)) :height height :pos (:pos e)
      :on-ground? (boolean (:on-ground e)) :ctx (phys/context e)
      :in-water? (boolean (:wet? e)))))

(defn done?
  "Returns true when the mob has no path node left to walk to."
  [e]
  (Nav/walked (:nav e)))

(defn- can-update-path? [e]
  (boolean (or (:on-ground e) (control/in-liquid? e))))

(defn stop
  "Returns mob e with its path dropped."
  [e]
  (if (:path (:nav e)) (assoc-in e [:nav :path] nil) e))

(defn- air-at? [chunks ^long x ^long y ^long z]
  (block/air? (chunk/block-state chunks x y z)))

(defn- solid-at? [chunks ^long x ^long y ^long z]
  (block/solid? (chunk/block-state chunks x y z)))

(defn- loaded? [chunks ^long x ^long z]
  (let [cx (bit-shift-right x 4)
        cz (bit-shift-right z 4)]
    (not (identical? chunk/empty-chunk
                     (chunk/chunk-at chunks cx cz)))))

(defn- above-air ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) hi (chunk/level-max-y lv)]
    (loop [cy (inc y)]
      (if (and (<= cy hi) (air-at? chunks x cy z))
        (recur (inc cy))
        cy))))

(defn- column-y ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) lo (chunk/level-min-y lv)]
    (loop [cy (dec y)]
      (cond (< cy lo) (above-air lv x y z)
            (air-at? chunks x cy z) (recur (dec cy))
            :else (inc cy)))))

(defn- above-solid ^long [lv ^long x ^long y ^long z]
  (let [chunks (:chunks lv) hi (chunk/level-max-y lv)]
    (loop [cy (inc y)]
      (if (and (<= cy hi) (solid-at? chunks x cy z))
        (recur (inc cy))
        cy))))

(defn- surface-cell
  [lv [x y z]]
  (let [chunks (:chunks lv) x (long x) z (long z)
        y (long (if (air-at? chunks x (long y) z)
                  (column-y lv x (long y) z)
                  y))]
    (if (solid-at? chunks x y z)
      [x (above-solid lv x y z) z]
      [x y z])))

(defn- search [world e cell ^long reach]
  (path/find-path world (walker e) #{cell} (path-length e) reach
                  1.0))

(defn- searched [world e cell ^long reach]
  (let [p (search world e cell reach)
        nav {:target (:target p) :reach reach
             :timeout-node [0 0 0] :timeout-timer 0
             :timeout-limit 0.0}]
    [(update e :nav merge nav) p]))

(defn- keep-path? [e cell nav]
  (and (:path nav) (not (done? e)) (= cell (:target nav))))

(defn- pathed [world e cell ^long reach]
  (let [nav (:nav e)]
    (cond
      (< (v/y (:pos e)) (chunk/level-min-y world)) [e nil]
      (not (can-update-path? e)) [e nil]
      (keep-path? e cell nav) [e (:path nav)]
      :else (searched world e cell reach))))

(defn- create-path [world e cell ^long reach]
  (let [chunks (:chunks world)
        e (assoc e :nav (nav-of e))
        [gx _ gz] cell]
    (if (loaded? chunks gx gz)
      (pathed world e (surface-cell world cell) reach)
      [e nil])))

(defn- cauldron? [chunks n]
  (block/tagged? (chunk/block-state chunks (:x n) (:y n) (:z n))
                 "cauldrons"))

(defn- raised [v ^long i]
  (let [n (nth v i) nx (get v (inc i))
        y (inc (long (:y n)))
        v (assoc v i (assoc n :y y))]
    (if (and nx (>= (long (:y n)) (long (:y nx))))
      (assoc v (inc i) (assoc nx :y y))
      v)))

(defn- trimmed [chunks p]
  (let [nodes (:nodes p)
        lift (fn [v i]
               (if (cauldron? chunks (nth v i)) (raised v i) v))]
    (assoc p :nodes (reduce lift nodes (range (count nodes))))))

(defn- temp-mob-pos [world e]
  (let [pos (:pos e)]
    [(v/x pos)
     (Nav/surfaceY (:chunks world) (block/tables) (v/x pos) (v/y pos)
                   (v/z pos) (boolean (:wet? e)))
     (v/z pos)]))

(defn- moved-to [world e p ^double speed]
  (let [nav (nav-of e)
        same? (= (:nodes p) (:nodes (:path nav)))
        e (assoc e :nav (if same? nav (assoc nav :path p :index 0)))]
    (if (done? e)
      e
      (let [trim (fn [q] (trimmed (:chunks world) q))
            e (update-in e [:nav :path] trim)]
        (update e :nav assoc :speed speed
                :stuck-check (:tick (:nav e))
                :stuck-pos (temp-mob-pos world e))))))

(defn- goal-cell [pos]
  (mapv (fn [c] (long (Math/floor (double c)))) pos))

(defn move-to
  "Returns mob e walking to the cell at speed.
  A goal it cannot reach leaves it without a path."
  [world e pos ^double speed]
  (let [[e p] (create-path world e (goal-cell pos) 1)]
    (if p
      (moved-to world e p speed)
      (stop (assoc e :nav (nav-of e))))))

(defn path-to
  "Returns mob e walking to the cell at speed, which it takes to be
  reached within reach, or nil when no path leads there."
  [world e pos speed reach]
  (let [[e p] (create-path world e (goal-cell pos) (long reach))]
    (when p (moved-to world e p (double speed)))))

(defn move-to-entity
  "Returns mob e walking to entity o at speed.
  A goal it cannot reach leaves the path it already walks."
  [world e o ^double speed]
  (let [[e p] (create-path world e (goal-cell (:pos o)) 1)]
    (if p (moved-to world e p speed) e)))

(defn- rebuilt [world e nav ^long t]
  (let [e (assoc-in (assoc e :nav nav) [:nav :path] nil)
        [e p] (create-path world e (:target nav) (:reach nav))]
    (update e :nav assoc :path p :index 0 :recompute t
            :delayed? false)))

(defn recompute-path
  "Returns mob e with its path to the same goal built again.
  Sooner than 20 ticks after the last one it only asks for a
  later recomputation."
  [world e]
  (let [nav (nav-of e) t (long (:tick world))]
    (cond
      (or (<= (- t (long (:recompute nav))) recompute-gap)
          (not (can-update-path? e)))
      (assoc e :nav (assoc nav :delayed? true))
      (nil? (:target nav)) (assoc e :nav nav)
      :else (rebuilt world e nav t))))

(defn cut-corner?
  "Returns true when a node of type t may be walked past on a corner."
  [t]
  (Nav/cutCorner t))

(defn- stepped [world e nav]
  (let [pos (:pos e) x (v/x pos) y (v/y pos) z (v/z pos)]
    (Nav/stepped nav (:chunks world) (block/tables) x y z
                 (boolean (:on-ground e)) (boolean (:wet? e))
                 (can-update-path? e) (half-of e)
                 (double (:speed (:move e) 0.0))
                 (long (:tick world)))))

(defn- aimed [world e nav]
  (Nav/aimed nav (:move e) (:chunks world) (block/collision-arr)
             (half-of e)))

(defn- walked-on [world e nav0 nav]
  (if (Nav/walked nav)
    [(if (identical? nav nav0) e (assoc e :nav nav)) nil nil]
    (let [nav2 (stepped world e nav)]
      (cond (not (Nav/walked nav2)) [e nav2 (aimed world e nav2)]
            (identical? nav0 nav2) [e nil nil]
            :else [(assoc e :nav nav2) nil nil]))))

(defn aim
  "Returns mob e after one tick of its navigation, then the path
  state and the move it aims at when it walks a path, or nils.
  The caller sets both on the mob."
  [world e]
  (let [nav0 (:nav e)
        nav (Nav/ticked nav0)]
    (if (:delayed? nav)
      (let [e (recompute-path world (assoc e :nav nav))]
        (walked-on world e (:nav e) (:nav e)))
      (walked-on world e nav0 nav))))

(defn tick
  "Returns mob e after one tick of its navigation.
  The mob walks its path on and tells its move control where to go."
  [world e]
  (let [[e nav m] (aim world e)]
    (if nav (entity/with e {:nav nav :move m}) e)))
