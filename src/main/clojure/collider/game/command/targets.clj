(ns collider.game.command.targets
  "The entities a parsed selector picks in the world, and the level and
  position a command source stands in."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.command.snbt :as snbt]
            [collider.game.entity :as entity]
            [collider.game.entity.save-data :as save-data]
            [collider.game.experience :as xp]
            [collider.game.level :as level]
            [collider.game.schema :as schema]
            [collider.num :as num]
            [collider.random :as random]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn source-dim
  "Returns the dimension a command runs in."
  [world]
  (get-in world [:source :dim] (:dim world)))

(defn source-pos
  "Returns the position a command runs at."
  [world]
  (get-in world [:source :pos]))

(defn level-view
  "Returns the level of dimension dim seen from world."
  [world dim]
  (let [server (:server world)]
    (if (or (= dim (:dim world)) (nil? server))
      world
      (assoc (level/level server dim) :server server))))

(defn in-level
  "Returns deltas ds for the level of dimension dim, nil when none."
  [world dim ds]
  (cond
    (empty? ds) nil
    (= dim (:dim world)) (vec ds)
    :else [[:level-deltas dim (vec ds)]]))

(defn source-level
  "Returns the level a command runs in."
  [world]
  (level-view world (source-dim world)))

(defn- levels [world]
  (if (:server world)
    (map (fn [d] [d (level-view world d)]) schema/dims)
    [[(:dim world) world]]))

(defn player-entries
  "Returns the players of every level as [eid dim e] by eid."
  [world]
  (sort-by first
           (for [[dim lv] (levels world)
                 [id e] (level/player-entries lv)]
             [id dim e])))

(defn- entity-entries [world]
  (for [[dim lv] (levels world)
        [id e] (sort-by key (:entities lv))]
    [id dim e]))

(defn- alive? [e]
  (not (and (:health e) (<= (double (:health e)) 0.0))))

(defn- dist-sq ^double [from e]
  (let [p (:pos e)
        d #(- (double %1) (double (nth from %2)))
        dx (d (v/x p) 0) dy (d (v/y p) 1) dz (d (v/z p) 2)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- in-distance? [[lo hi] from e]
  (let [d (dist-sq from e) sq #(* (double %) (double %))]
    (and (or (nil? lo) (>= d (double (sq lo))))
         (or (nil? hi) (<= d (double (sq hi)))))))

(defn- in-bounds? [[lo hi] v]
  (and (or (nil? lo) (>= (double v) (double lo)))
       (or (nil? hi) (<= (double v) (double hi)))))

(defn wrapped
  "Returns angle a in degrees as a float from -180 up to 180."
  [a]
  (num/wrap-degrees a))

(defn- turned? [[lo hi] rot]
  (let [a (wrapped (or lo 0.0)) b (wrapped (or hi 359.0))
        r (wrapped rot)]
    (if (> a b) (or (>= r a) (<= r b)) (and (>= r a) (<= r b)))))

(defn- type-tagged? [id t]
  (let [tag (get (data/tags) "entity_type")]
    (some #{t} (get tag (str/replace id #"^minecraft:" "")))))

(defn- tagged? [tag e]
  (if (= "" tag) (empty? (:tags e)) (contains? (:tags e) tag)))

(defn- pred? [[k a inv] e t]
  (let [f #(not= (boolean inv) (boolean %))]
    (case k
      :alive (alive? e)
      :name (f (= a (when (= :player (:type e)) (:name e))))
      :gamemode (and (= :player (:type e)) (f (= a (:game-mode e))))
      :team (f (= a ""))
      :type (f (= a (:type e)))
      :type-tag (f (type-tagged? a (:type e)))
      :tag (f (tagged? a e))
      :nbt (f (snbt/compare-nbt a (save-data/saved e t) true))
      false)))

(defn xp-of
  "Returns the experience of player e in world."
  [world e]
  (xp/account e (- (long (:tick world)) (long (:born e 0)))))

(defn- leveled? [world sel e]
  (or (nil? (:level sel))
      (and (= :player (:type e))
           (in-bounds? (:level sel) (:level (xp-of world e))))))

(defn- entity-box [e]
  (let [[hw h] (entity/box e) p (:pos e)
        x (v/x p) y (v/y p) z (v/z p) hw (double hw)]
    [[(- x hw) (+ x hw)] [y (+ y (double h))] [(- z hw) (+ z hw)]]))

(defn- overlaps? [a b]
  (every? (fn [[[a0 a1] [b0 b1]]]
            (and (< (double a0) (double b1))
                 (> (double a1) (double b0))))
          (map vector a b)))

(defn- sel-pos [world sel]
  (let [p (source-pos world) o (:pos sel)]
    (mapv #(double (get o % (nth p %))) [0 1 2])))

(defn- delta-box [d pos]
  (mapv (fn [i]
          (let [v (double (get d i 0.0)) c (double (pos i))]
            [(+ c (min v 0.0)) (+ c (max v 0.0) 1.0)]))
        [0 1 2]))

(defn- sel-box [sel pos]
  (let [d (:delta sel) hi (second (:range sel))]
    (cond
      (seq d) (delta-box d pos)
      hi (mapv #(vector (- (double %) (double hi))
                        (+ (double %) (double hi) 1.0))
               pos))))

(defn- selects? [world sel pos box [_ _ e]]
  (and (every? #(pred? % e (:tick world)) (:preds sel))
       (or (nil? (:type sel)) (= (:type sel) (:type e)))
       (or (nil? (:rot-x sel)) (turned? (:rot-x sel) (:pitch e 0.0)))
       (or (nil? (:rot-y sel)) (turned? (:rot-y sel) (:yaw e 0.0)))
       (leveled? world sel e)
       (or (nil? box) (overlaps? box (entity-box e)))
       (or (nil? (:range sel)) (in-distance? (:range sel) pos e))))

(defn- random-key [world]
  #(random/of-key [(:tick world) :selector (first %)]))

(defn- ordered [world sel pos xs]
  (case (:order sel)
    :nearest (sort-by #(dist-sq pos (nth % 2)) xs)
    :furthest (sort-by #(- (dist-sq pos (nth % 2))) xs)
    :random (sort-by (random-key world) xs)
    xs))

(defn by-uuid
  "Returns the entity of uuid u as [eid dim e], only among the
  players when players? is true."
  [world players? u]
  (some (fn [[id _ e :as x]]
          (when (= u (entity/uuid-of id e)) x))
        (if players? (player-entries world) (entity-entries world))))

(defn- by-name [world nm]
  (some #(when (= nm (:name (nth % 2))) %) (player-entries world)))

(defn- self-entry [world eid sel ok?]
  (let [x [eid (:dim world) (get-in world [:entities eid])]]
    (when (and (peek x) (ok? x)
               (or (:entities? sel) (= :player (:type (peek x)))))
      [x])))

(defn- candidates [world sel]
  (let [xs (if (:entities? sel)
             (entity-entries world)
             (player-entries world))
        dim (source-dim world)]
    (if (:world? sel) (filter #(= dim (second %)) xs) xs)))

(defn selected
  "Returns the entities that selector sel of player eid selects, each
  as [eid dim e]."
  [world eid sel]
  (let [pos (sel-pos world sel) box (sel-box sel pos)
        ok? #(selects? world sel pos box %)]
    (cond
      (:name sel) (keep identity [(by-name world (:name sel))])
      (:uuid sel) (keep identity [(by-uuid world false (:uuid sel))])
      (:self? sel) (self-entry world eid sel ok?)
      :else (->> (filter ok? (candidates world sel))
                 (ordered world sel pos)
                 (take (:limit sel))))))

(defn player-selected
  "Returns the players that selector sel of player eid selects, each
  as [eid dim e]."
  [world eid sel]
  (if (:uuid sel)
    (keep identity [(by-uuid world true (:uuid sel))])
    (selected world eid sel)))

(defn self
  "Returns entity eid as its own selection."
  [world eid]
  (selected world eid {:self? true :entities? true :limit 1}))
