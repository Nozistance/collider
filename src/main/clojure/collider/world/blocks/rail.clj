(ns collider.world.blocks.rail
  "Rails that join into curves and slopes and go when unheld."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def rail-types
  "The block types of rails."
  #{:rail :powered-rail :detector-rail})

(defn rail?
  "Returns true when st is a rail of any kind."
  [^long st]
  (contains? rail-types (block/type-of st)))

(defn- curveless? [^long st] (not= :rail (block/type-of st)))

(defn- shape-of [^long st] (:shape (block/props-of st)))

(defn- shaped ^long [^long st shape] (block/with st :shape shape))

(def ^:private link-offsets
  (let [{:keys [north south west east]} dir/offset]
    {:north_south [north south]
     :east_west [west east]
     :ascending_east [west [1 1 0]]
     :ascending_west [[-1 1 0] east]
     :ascending_north [[0 1 -1] south]
     :ascending_south [north [0 1 1]]
     :south_east [east south]
     :south_west [west south]
     :north_west [west north]
     :north_east [east north]}))

(def ^:private slope-sides
  {:ascending_east :east :ascending_west :west
   :ascending_north :north :ascending_south :south})

(def ^:private sides [:north :south :west :east])

(defn- linked [p shape]
  (mapv #(mapv + p %) (link-offsets shape)))

(defn- rail-at [chunks p]
  (let [st (chunk/at chunks p)]
    {:pos p :st st :links (linked p (shape-of st))}))

(defn- found
  "Returns the rail at p, above p or below p, in that order."
  [chunks p]
  (some #(when (rail? (chunk/at chunks %)) (rail-at chunks %))
        [p (dir/up p) (dir/down p)]))

(defn- has-link? [links [x _ z]]
  (boolean (some (fn [[lx _ lz]] (and (= lx x) (= lz z))) links)))

(defn- links-to? [a b] (has-link? (:links a) (:pos b)))

(defn- linked-back [chunks r q]
  (when-let [n (found chunks q)]
    (when (links-to? n r) (:pos n))))

(defn- firm
  "Returns the rail r with only the links that a rail links back."
  [chunks r]
  (assoc r :links
         (into [] (keep #(linked-back chunks r %)) (:links r))))

(defn- may-link? [a b]
  (or (links-to? a b) (not= 2 (count (:links a)))))

(defn- rail-near? [chunks p]
  (some #(rail? (chunk/at chunks %)) [p (dir/up p) (dir/down p)]))

(defn- potential-links
  "Returns how many sides of p have a rail at, above or below them."
  ^long [chunks p]
  (count (filter #(rail-near? chunks (dir/toward p %))
                 [:north :east :south :west])))

(defn- rail-above? [chunks p side]
  (rail? (chunk/at chunks (dir/up (dir/toward p side)))))

(defn- sloped
  "Returns shape raised toward a rail one block up on its axis."
  [chunks p shape]
  (case shape
    :north_south (cond (rail-above? chunks p :south) :ascending_south
                       (rail-above? chunks p :north) :ascending_north
                       :else shape)
    :east_west (cond (rail-above? chunks p :west) :ascending_west
                     (rail-above? chunks p :east) :ascending_east
                     :else shape)
    shape))

(defn- curve [n s w e]
  (cond (and s e (not n) (not w)) :south_east
        (and s w (not n) (not e)) :south_west
        (and n w (not s) (not e)) :north_west
        (and n e (not s) (not w)) :north_east))

(defn- joined-shape
  "Returns the shape of rail a when it also links to b."
  [chunks a b]
  (let [p (:pos a)
        ls (conj (:links a) (:pos b))
        [n s w e] (map #(has-link? ls (dir/toward p %)) sides)
        shape (or (when-not (curveless? (:st a)) (curve n s w e))
                  (cond (or w e) :east_west (or n s) :north_south))]
    (or (sloped chunks p shape) :north_south)))

(defn- joined [chunks q r]
  (when-let [n (found chunks q)]
    (let [n (firm chunks n)]
      (when (may-link? n r)
        [[(:pos n) (shaped (:st n) (joined-shape chunks n r))]]))))

(defn- neighbor-rail? [chunks r q]
  (when-let [n (found chunks q)]
    (may-link? (firm chunks n) r)))

(defn- junction [signal? [n s w e] shape]
  (let [se (and s e) sw (and s w) ne (and n e) nw (and n w)]
    (if signal?
      (cond nw :north_west ne :north_east sw :south_west
            se :south_east :else shape)
      (cond se :south_east sw :south_west ne :north_east
            nw :north_west :else shape))))

(defn- crossing [curveless? signal? near default]
  (if curveless? default (junction signal? near default)))

(defn- shape-for [curveless? signal? [n s w e :as near] default]
  (let [ns (or n s) we (or w e)]
    (or (when-not curveless? (curve n s w e))
        (cond (and ns we) (crossing curveless? signal? near default)
              ns :north_south
              we :east_west))))

(defn- chosen-shape [chunks r curveless? signal? default]
  (let [p (:pos r)
        near (map #(neighbor-rail? chunks r (dir/toward p %)) sides)
        shape (shape-for curveless? signal? near default)]
    (or (sloped chunks p shape) default)))

(defn- placed
  "Returns the changes of rail st at p as it takes its shape from the
  rails around it and joins them. A powered junction curves the other
  way. A rail that is not first is set only when its shape changes."
  [chunks p st signal? first?]
  (let [r (rail-at chunks p)
        default (shape-of st)
        shape (chosen-shape chunks r (curveless? st) signal? default)
        st' (shaped st shape)]
    (when (or first? (not= st' (chunk/at chunks p)))
      (into [[p st']]
            (map (fn [q] #(joined % q r)))
            (linked p shape)))))

(defn- rigid-top? [chunks p]
  (block/face-holds-rigid? (chunk/at chunks p) :up))

(defn- removed?
  "Returns true when nothing holds a rail of shape at p."
  [chunks p shape]
  (or (not (rigid-top? chunks (dir/down p)))
      (when-let [side (slope-sides shape)]
        (not (rigid-top? chunks (dir/toward p side))))))

(defn- self-told [p ^long st]
  (fn [chunks]
    [[p (chunk/at chunks p) [[:neighbor-changed p st]]]]))

(defn- set-down
  "Returns the changes of rail st set at p."
  [chunks p ^long st]
  (let [cs (placed chunks p st (signal/has-neighbor-signal? chunks p)
                   true)
        st' (or (some-> cs first second) st)]
    (cond-> (vec cs) (curveless? st) (conj (self-told p st')))))

(defn- signal-block?
  "Returns true when the block of old is a signal source in its
  default state."
  [^long old]
  (block/signal-source? (block/state (block/block-of old))))

(defn- rechecked
  "Returns the changes of rail st after block old beside it changes."
  [chunks p ^long st ^long old]
  (cond
    (removed? chunks p (shape-of st))
    [[p (block/emptied st) [[:drop st]]]]
    (and (not (curveless? st)) (signal-block? old)
         (= 3 (potential-links chunks p)))
    (placed chunks p st (signal/has-neighbor-signal? chunks p)
            false)))

(defn- same-block? [^long a ^long b]
  (= (block/block-of a) (block/block-of b)))

(defn- on-neighbor-change [chunks p {:keys [side old]}]
  (let [st (chunk/at chunks p) old (long old)]
    (cond
      (some? side) (rechecked chunks p st old)
      (not (same-block? st old)) (set-down chunks p st)
      (= st old) (rechecked chunks p st st))))

(defn removal-notified
  "Returns the cells whose neighbours hear that rail st at p went."
  [p ^long st]
  (cond-> []
    (contains? slope-sides (shape-of st)) (conj (dir/up p))
    (curveless? st) (conj p (dir/down p))))

(def rule
  "The block rule of rails."
  {:name    :rail
   :match?  (fn [_chunks st _p] (rail? st))
   :pass    :neighbor
   :wake    (constantly :neighbor)
   :reshape on-neighbor-change})
