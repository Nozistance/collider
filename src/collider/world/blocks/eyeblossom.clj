(ns collider.world.blocks.eyeblossom
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? y) (chunk/chunks-get-block chunks gen/flat-chunk p) 0))

(defn eyeblossom? [^long st] (= :eyeblossom (block/type-of st)))
(defn- night? [^long time] (<= 12600 (mod time 24000) 23400))
(defn switched [^long st ^long time]
  (let [open? (= :open-eyeblossom (block/block-of st))]
    (when (not= open? (night? time))
      (block/state (if (night? time) :open-eyeblossom :closed-eyeblossom)))))

(defn sound-kind [^long st long?]
  (let [open? (= :open-eyeblossom (block/block-of st))]
    (if long?
      (if open? :eyeblossom/open-long :eyeblossom/close-long)
      (if open? :eyeblossom/open :eyeblossom/close))))

(defn- neighbours [[x y z]]
  (for [dx (range -3 4) dy (range -2 3) dz (range -3 4)
        :when (not (and (zero? (long dx)) (zero? (long dy)) (zero? (long dz))))]
    [(+ (long x) (long dx)) (+ (long y) (long dy)) (+ (long z) (long dz))]))

(defn cascade [chunks [x y z :as p] ^long old ^long tick]
  (reduce (fn [m [qx qy qz :as q]]
            (if (not= old (at chunks q))
              m
              (let [dx (- (long qx) (long x)) dy (- (long qy) (long y)) dz (- (long qz) (long z))
                    dist (Math/sqrt (double (+ (* dx dx) (* dy dy) (* dz dz))))
                    lo (long (* dist 5.0)) hi (long (* dist 10.0))
                    roll (long (Math/floor (* (random/of-key [tick q :eyeblossom]) (inc (- hi lo)))))]
                (update m (+ tick lo roll) (fnil conj []) (chunk/block-pos->id q)))))
          {} (neighbours p)))

(def rule
  {:name   :eyeblossom
   :match? (fn [_chunks st _p] (eyeblossom? st))
   
   
   
   :wake   (fn [chunks tick p _old _self?]
             (when-not (support/supported? chunks gen/flat-chunk p
                                           (chunk/chunks-get-block chunks gen/flat-chunk p))
               (inc (long tick))))
   :due    (fn [chunks p ctx]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (or (seq ((:due support/rule) chunks p nil))
                   (when-let [new (switched st (long (:time-of-day ctx 0)))]
                     [[p new]]))))})
