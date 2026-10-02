(ns collider.world.blocks.grow.mushroom
  "Spreading mushrooms, and the huge mushrooms bone meal makes."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.blocks.grow.common
             :refer [air-at? chance? crowd]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- spread-step [roll q i]
  (let [dx (dec (random/below (roll [:x i]) 3))
        dy (- (random/below (roll [:y1 i]) 2)
              (random/below (roll [:y2 i]) 2))
        dz (dec (random/below (roll [:z i]) 3))]
    (mapv + q [dx dy dz])))

(defn- spread-target [roll fits? p]
  (loop [q p off (spread-step roll p 0) i 1]
    (if (> i 4)
      off
      (let [q (if (fits? off) off q)]
        (recur q (spread-step roll q i) (inc i))))))

(defn tick
  "Returns the mushroom a random tick of the mushroom st at p
  spreads to, if any."
  [chunks p st roll _time _world]
  (when (and (chance? roll :gate 25)
             (< (crowd chunks p (block/block-of st)) 5))
    (let [fits? #(and (air-at? chunks %)
                      (support/supported? chunks % st))
          target (spread-target roll fits? p)]
      (when (fits? target) [[target st]]))))

(def ^:private huge
  {:red-mushroom
   {:cap :red-mushroom-block :radius 2
    :tag "huge_red_mushroom_can_place_on"}
   :brown-mushroom
   {:cap :brown-mushroom-block :radius 3
    :tag "huge_brown_mushroom_can_place_on"}})

(def ^:private ^:table stem-state
  (delay (block/state :mushroom-stem {:up :false :down :false})))

(defn- state-ignoring-origin ^long [chunks origin q]
  (if (= q origin) 0 (chunk/at chunks q)))

(defn- room-radius ^long [kind ^long radius ^long dy]
  (if (= :brown-mushroom kind) (if (<= dy 3) 0 radius) 0))

(defn- room? [chunks p kind radius height]
  (every? (fn [off]
            (let [st (state-ignoring-origin chunks p (mapv + p off))]
              (or (zero? st) (block/tagged? st "leaves"))))
          (for [dy (range (inc (long height)))
                :let [r (room-radius kind (long radius) dy)]
                dx (range (- r) (inc r)) dz (range (- r) (inc r))]
            [dx dy dz])))

(defn- fits? [chunks [_ y _ :as p] kind radius tag height]
  (and (>= (long y) 1) (chunk/in-range? (+ (long y) (long height) 1))
       (block/tagged? (chunk/at chunks (dir/down p)) tag)
       (room? chunks p kind (long radius) (long height))))

(defn- height-roll ^long [roll]
  (let [h (+ 4 (random/below (roll :height) 3))]
    (if (zero? (random/below (roll :double) 12)) (* 2 h) h)))

(defn- red-faces [cap ^long center [dx dy dz] ^long h]
  (let [dx (long dx) dy (long dy) dz (long dz)]
    (block/state cap {:down :false
                      :up (block/flag (>= dy (dec h)))
                      :west (block/flag (< dx (- center)))
                      :east (block/flag (> dx center))
                      :north (block/flag (< dz (- center)))
                      :south (block/flag (> dz center))})))

(defn- red-cap [p cap ^long radius ^long height]
  (let [center (- radius 2)]
    (for [dy (range (- height 3) (inc height))
          :let [r (if (< dy height) radius (dec radius))]
          dx (range (- r) (inc r)) dz (range (- r) (inc r))
          :let [xe (or (= dx (- r)) (= dx r))
                ze (or (= dz (- r)) (= dz r))
                off [dx dy dz]]
          :when (or (>= dy height) (not= xe ze))]
      [(mapv + p off) (red-faces cap center off height)])))

(defn- brown-faces [cap ^long r ^long dx ^long dz]
  (let [nx (= dx (- r)) px (= dx r) nz (= dz (- r)) pz (= dz r)
        xe (or nx px) ze (or nz pz)
        f #(block/flag (or %1 (and %2 (= %3 %4))))]
    (block/state cap {:up :true :down :false
                      :west (f nx ze dx (- 1 r))
                      :east (f px ze dx (dec r))
                      :north (f nz xe dz (- 1 r))
                      :south (f pz xe dz (dec r))})))

(defn- corner? [^long r ^long dx ^long dz]
  (and (or (= dx (- r)) (= dx r)) (or (= dz (- r)) (= dz r))))

(defn- brown-cap [p cap ^long radius ^long height]
  (for [dx (range (- radius) (inc radius))
        dz (range (- radius) (inc radius))
        :when (not (corner? radius dx dz))]
    [(mapv + p [dx height dz]) (brown-faces cap radius dx dz)]))

(defn- cells [p kind cap radius height]
  (let [cap-fn (if (= :brown-mushroom kind) brown-cap red-cap)]
    (concat (cap-fn p cap (long radius) (long height))
            (for [dy (range (long height))]
              [(dir/toward p :up dy) @stem-state]))))

(defn- changes [chunks origin cells]
  (loop [cells (seq cells) seen {origin 0} acc []]
    (if-let [[q st] (first cells)]
      (let [cur (long (get seen q (chunk/at chunks q)))]
        (if (or (zero? cur)
                (block/tagged? cur "replaceable_by_mushrooms"))
          (recur (next cells) (assoc seen q st) (conj acc [q st]))
          (recur (next cells) seen acc)))
      acc)))

(defn- grown [chunks p kind roll]
  (let [{:keys [cap radius tag]} (huge kind) radius (long radius)
        height (height-roll roll)]
    (if (fits? chunks p kind radius tag height)
      (changes chunks p (cells p kind cap radius height))
      [])))

(defn meal
  "Returns the bone meal result for the mushroom st at p. The changes
  are the blocks of a huge mushroom, or empty when none fits there."
  [chunks [_ y _ :as p] st roll]
  (let [kind (block/block-of st) {:keys [radius]} (huge kind)]
    (when (and radius (chunk/in-range? (+ (long y) 4 (long radius))))
      {:changes (if (< (double (roll :success)) 0.4)
                  (grown chunks p kind roll)
                  [])})))
