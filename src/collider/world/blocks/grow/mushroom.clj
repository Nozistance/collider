(ns collider.world.blocks.grow.mushroom
  (:require [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.gen :as gen]
            [collider.world.blocks.grow.common :refer [air-at? chance? crowd flag pick]]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn tick [chunks p st roll _time _ctx]
  (when (and (chance? roll :gate 25) (< (crowd chunks p (block/block-of st)) 5))
    (let [step (fn [q i] (mapv + q [(dec (pick roll [:x i] 3)) (- (pick roll [:y1 i] 2) (pick roll [:y2 i] 2)) (dec (pick roll [:z i] 3))]))
          ok? (fn [q] (and (air-at? chunks q) (support/supported? chunks gen/flat-chunk q st)))
          target (loop [q p off (step p 0) i 1]
                   (if (> i 4)
                     off
                     (let [q (if (ok? off) off q)]
                       (recur q (step q i) (inc i)))))]
      (when (ok? target) [[target st]]))))

(def ^:private huge
  {:red-mushroom   {:cap :red-mushroom-block :radius 2 :tag "huge_red_mushroom_can_place_on"}
   :brown-mushroom {:cap :brown-mushroom-block :radius 3 :tag "huge_brown_mushroom_can_place_on"}})
(def ^:private stem-state (block/state :mushroom-stem {:up :false :down :false}))

(defn- cleared-at ^long [chunks origin q]
  (if (= q origin) 0 (gen/at chunks q)))

(defn- check-radius ^long [kind ^long radius ^long dy]
  (if (= :brown-mushroom kind) (if (<= dy 3) 0 radius) 0))

(defn- room? [chunks p kind radius height]
  (every? (fn [[dx dy dz]]
            (let [st (cleared-at chunks p (mapv + p [dx dy dz]))]
              (or (zero? st) (block/tagged? st "leaves"))))
          (for [dy (range (inc (long height)))
                :let [r (check-radius kind (long radius) dy)]
                dx (range (- r) (inc r)) dz (range (- r) (inc r))]
            [dx dy dz])))

(defn- fits? [chunks [_ y _ :as p] kind radius tag height]
  (and (>= (long y) 1) (chunk/in-range? (+ (long y) (long height) 1))
       (block/tagged? (gen/at chunks (dir/down p)) tag)
       (room? chunks p kind (long radius) (long height))))

(defn- height-roll ^long [roll]
  (let [h (+ 4 (pick roll :height 3))]
    (if (zero? (pick roll :double 12)) (* 2 h) h)))

(defn- red-cap [p cap ^long radius ^long height]
  (let [center (- radius 2)]
    (for [dy (range (- height 3) (inc height))
          :let [r (if (< dy height) radius (dec radius))]
          dx (range (- r) (inc r)) dz (range (- r) (inc r))
          :let [xe (or (= dx (- r)) (= dx r)) ze (or (= dz (- r)) (= dz r))]
          :when (or (>= dy height) (not= xe ze))]
      [(mapv + p [dx dy dz])
       (block/state cap {:down  :false :up (flag (>= dy (dec height)))
                         :west  (flag (< dx (- center))) :east (flag (> dx center))
                         :north (flag (< dz (- center))) :south (flag (> dz center))})])))

(defn- brown-cap [p cap ^long radius ^long height]
  (for [dx (range (- radius) (inc radius)) dz (range (- radius) (inc radius))
        :let [nx (= dx (- radius)) px (= dx radius) nz (= dz (- radius)) pz (= dz radius)
              xe (or nx px) ze (or nz pz)]
        :when (not (and xe ze))]
    [(mapv + p [dx height dz])
     (block/state cap {:up    :true :down :false
                       :west  (flag (or nx (and ze (= dx (- 1 radius)))))
                       :east  (flag (or px (and ze (= dx (dec radius)))))
                       :north (flag (or nz (and xe (= dz (- 1 radius)))))
                       :south (flag (or pz (and xe (= dz (dec radius)))))})]))

(defn- cells [p kind cap radius height]
  (let [cap-fn (if (= :brown-mushroom kind) brown-cap red-cap)]
    (concat (cap-fn p cap (long radius) (long height))
            (for [dy (range (long height))] [(mapv + p [0 dy 0]) stem-state]))))

(defn- changes [chunks origin cells]
  (loop [cells (seq cells) seen {origin 0} acc []]
    (if-let [[q st] (first cells)]
      (let [cur (long (get seen q (gen/at chunks q)))]
        (if (or (zero? cur) (block/tagged? cur "replaceable_by_mushrooms"))
          (recur (next cells) (assoc seen q st) (conj acc [q st]))
          (recur (next cells) seen acc)))
      acc)))

(defn- grown [chunks p kind roll]
  (let [{:keys [cap radius tag]} (huge kind) radius (long radius)
        height (height-roll roll)]
    (if (fits? chunks p kind radius tag height)
      (changes chunks p (cells p kind cap radius height))
      [])))

(defn meal [chunks [_ y _ :as p] st roll]
  (let [kind (block/block-of st) {:keys [radius]} (huge kind)]
    (when (and radius (chunk/in-range? (+ (long y) 4 (long radius))))
      {:changes (if (< (double (roll :success)) 0.4) (grown chunks p kind roll) [])})))
