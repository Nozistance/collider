(ns collider.world.blocks.rail
  "Rails that join into curves and slopes and go when unheld."
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(def ^:private rail-types #{:rail :powered-rail :detector-rail})

(defn rail?
  "Returns true when st is a rail of any kind."
  [^long st]
  (contains? rail-types (block/type-of st)))

(defn- straight? [^long st] (not= :rail (block/type-of st)))

(defn- shape-of [^long st] (:shape (block/props-of st)))

(defn- shaped ^long [^long st shape]
  (block/state (block/block-of st)
               (assoc (block/props-of st) :shape shape)))

(defn- at ^long [chunks p]
  (if (chunk/in-range? (long (p 1)))
    (chunk/chunks-get-block chunks p)
    0))

(defn- moved [p d] (mapv + p d))

(def ^:private north [0 0 -1])

(def ^:private south [0 0 1])

(def ^:private west [-1 0 0])

(def ^:private east [1 0 0])

(def ^:private up [0 1 0])

(def ^:private down [0 -1 0])

(def ^:private link-offsets
  {:north_south [north south]
   :east_west [west east]
   :ascending_east [west [1 1 0]]
   :ascending_west [[-1 1 0] east]
   :ascending_north [[0 1 -1] south]
   :ascending_south [north [0 1 1]]
   :south_east [east south]
   :south_west [west south]
   :north_west [west north]
   :north_east [east north]})

(defn- slope? [shape]
  (contains? #{:ascending_east :ascending_west :ascending_north
               :ascending_south}
             shape))

(defn- linked [p shape]
  (mapv #(moved p %) (link-offsets shape)))

(defn- rail-at [chunks p]
  (let [st (at chunks p)]
    {:pos p :st st :links (linked p (shape-of st))}))

(defn- found
  "The rail at p, else above it, else below it (RailState.getRail)."
  [chunks p]
  (some #(when (rail? (at chunks %)) (rail-at chunks %))
        [p (moved p up) (moved p down)]))

(defn- has-link? [links [x _ z]]
  (boolean (some (fn [[lx _ lz]] (and (= lx x) (= lz z))) links)))

(defn- links-to? [a b] (has-link? (:links a) (:pos b)))

(defn- linked-back [chunks r q]
  (when-let [n (found chunks q)]
    (when (links-to? n r) (:pos n))))

(defn- firm
  "r without the links that lead to no rail linked back to it
  (RailState.removeSoftConnections)."
  [chunks r]
  (assoc r :links
         (into [] (keep #(linked-back chunks r %)) (:links r))))

(defn- may-link? [a b]
  (or (links-to? a b) (not= 2 (count (:links a)))))

(defn- rail-near? [chunks p]
  (some #(rail? (at chunks %)) [p (moved p up) (moved p down)]))

(defn potential-links
  "Returns how many sides of p have a rail at, above or below them
  (RailState.countPotentialConnections)."
  ^long [chunks p]
  (count (filter #(rail-near? chunks (moved p %))
                 [north east south west])))

(defn- rail-above? [chunks p d]
  (rail? (at chunks (moved (moved p d) up))))

(defn- sloped
  "shape raised towards a rail one up on its axis; the later check
  of vanilla wins."
  [chunks p shape]
  (case shape
    :north_south (cond (rail-above? chunks p south) :ascending_south
                       (rail-above? chunks p north) :ascending_north
                       :else shape)
    :east_west (cond (rail-above? chunks p west) :ascending_west
                     (rail-above? chunks p east) :ascending_east
                     :else shape)
    shape))

(defn- curve [n s w e]
  (cond (and s e (not n) (not w)) :south_east
        (and s w (not n) (not e)) :south_west
        (and n w (not s) (not e)) :north_west
        (and n e (not s) (not w)) :north_east))

(defn- joined-shape
  "The shape of rail a linked to b as well (RailState.connectTo)."
  [chunks a b]
  (let [p (:pos a)
        ls (conj (:links a) (:pos b))
        [n s w e] (map #(has-link? ls (moved p %))
                       [north south west east])
        shape (or (when-not (straight? (:st a)) (curve n s w e))
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

(defn- open-shape [straight? signal? [n s w e :as sides] default]
  (let [ns (or n s) we (or w e)
        shape (cond (and ns we) default ns :north_south
                    we :east_west)]
    (if straight? shape (junction signal? sides shape))))

(defn- chosen-shape
  [chunks r straight? signal? default]
  (let [p (:pos r)
        sides (map #(neighbor-rail? chunks r (moved p %))
                   [north south west east])
        [n s w e] sides
        ns (or n s) we (or w e)
        shape (or (when-not straight? (curve n s w e))
                  (cond (and ns (not we)) :north_south
                        (and we (not ns)) :east_west))
        shape (or shape (open-shape straight? signal? sides default))]
    (or (sloped chunks p shape) default)))

(defn placed
  "Returns the changes of the rail st at p taking its shape from the
  rails around it and joining them to it (RailState.place). When
  first? is false the rail is set only when its shape changes.
  signal? tells whether p is powered, which picks the curve of a
  junction."
  [chunks p st signal? first?]
  (let [r (rail-at chunks p)
        default (shape-of st)
        shape (chosen-shape chunks r (straight? st) signal? default)
        st' (shaped st shape)]
    (when (or first? (not= st' (at chunks p)))
      (into [[p st']]
            (map (fn [q] #(joined % q r)))
            (linked p shape)))))

(defn- rigid-top? [chunks p]
  (block/face-holds-rigid? (at chunks p) :up))

(def ^:private slope-sides
  {:ascending_east east :ascending_west west
   :ascending_north north :ascending_south south})

(defn- removed?
  "Tells whether a rail of shape at p has lost what holds it
  (BaseRailBlock.shouldBeRemoved)."
  [chunks p shape]
  (or (not (rigid-top? chunks (moved p down)))
      (when-let [d (slope-sides shape)]
        (not (rigid-top? chunks (moved p d))))))

(defn- self-told [p ^long st]
  (fn [chunks]
    [[p (at chunks p) [[:neighbor-changed p st]]]]))

(defn- set-down
  "The changes of the rail st placed at p (BaseRailBlock.onPlace):
  its shape, then a straight rail tells itself of a change."
  [chunks p ^long st]
  (let [cs (placed chunks p st (signal/has-neighbor-signal? chunks p)
                   true)
        st' (or (some-> cs first second) st)]
    (cond-> (vec cs) (straight? st) (conj (self-told p st')))))

(defn- signal-block?
  "Tells whether the block of old is a signal source in its default
  state, as Block.defaultBlockState().isSignalSource() asks."
  [^long old]
  (block/signal-source? (block/state (block/block-of old))))

(defn- rechecked
  "The changes of a rail told that the block old beside it changed
  (BaseRailBlock.neighborChanged): it goes when it has lost its
  hold, else a plain rail at a junction of three shapes itself anew
  when old is a signal source."
  [chunks p ^long st ^long old]
  (cond
    (removed? chunks p (shape-of st))
    [[p (block/emptied st) [[:drop st]]]]
    (and (not (straight? st)) (signal-block? old)
         (= 3 (potential-links chunks p)))
    (placed chunks p st (signal/has-neighbor-signal? chunks p)
            false)))

(defn- same-block? [^long a ^long b]
  (= (block/block-of a) (block/block-of b)))

(defn- reply [chunks p {:keys [side old]}]
  (let [st (at chunks p) old (long old)]
    (cond
      (some? side) (rechecked chunks p st old)
      (not (same-block? st old)) (set-down chunks p st)
      (= st old) (rechecked chunks p st st))))

(defn removal-notified
  "Returns the cells whose neighbours hear that the rail st at p went
  (BaseRailBlock.affectNeighborsAfterRemoval)."
  [p ^long st]
  (cond-> []
    (slope? (shape-of st)) (conj (moved p up))
    (straight? st) (conj p (moved p down))))

(def rule
  {:name    :rail
   :match?  (fn [_chunks st _p] (rail? st))
   :pass    :neighbor
   :wake    (fn [_chunks _dim _tick _p _old _side] :neighbor)
   :reshape reply})
