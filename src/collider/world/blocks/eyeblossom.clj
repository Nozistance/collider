(ns collider.world.blocks.eyeblossom
  "Eyeblossoms, open by night and closed by day."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn eyeblossom? [^long st] (= :eyeblossom (block/type-of st)))

(defn- night? [^long time] (<= 12600 (mod time 24000) 23400))

(defn switched [^long st ^long time]
  (let [open? (= :open-eyeblossom (block/block-of st))]
    (when (not= open? (night? time))
      (block/state (if (night? time)
                     :open-eyeblossom
                     :closed-eyeblossom)))))

(defn sound-kind [^long st long?]
  (let [open? (= :open-eyeblossom (block/block-of st))]
    (if long?
      (if open? :eyeblossom/open-long :eyeblossom/close-long)
      (if open? :eyeblossom/open :eyeblossom/close))))

(defn- neighbours [[x y z]]
  (for [dx (range -3 4) dy (range -2 3) dz (range -3 4)
        :when (not (and (zero? (long dx)) (zero? (long dy))
                        (zero? (long dz))))]
    [(+ (long x) (long dx)) (+ (long y) (long dy))
     (+ (long z) (long dz))]))

(defn- follow-tick ^long [[x y z] [qx qy qz :as q] ^long tick]
  (let [dx (- (long qx) (long x))
        dy (- (long qy) (long y))
        dz (- (long qz) (long z))
        dist (Math/sqrt (double (+ (* dx dx) (* dy dy) (* dz dz))))
        lo (long (* dist 5.0)) hi (long (* dist 10.0))
        r (random/of-key tick q :eyeblossom)
        roll (long (Math/floor (* r (inc (- hi lo)))))]
    (+ tick lo roll)))

(defn cascade
  "Returns the eyeblossoms near p that follow the one at p.
  They are grouped by the tick they change on."
  [chunks p ^long old ^long tick]
  (reduce (fn [m q]
            (if (not= old (chunk/at chunks q))
              m
              (update m (follow-tick p q tick) (fnil conj [])
                      (chunk/block-pos->id q))))
          {} (neighbours p)))

(defn- wake [chunks _dim _tick p _old _side]
  (let [st (chunk/chunks-get-block chunks p)]
    (when-not (support/supported? chunks p st) :neighbor)))

(defn- switch-due [chunks p ctx]
  (let [st (chunk/chunks-get-block chunks p)
        time (long (:time-of-day ctx 0))]
    (when-let [new (switched st time)]
      (let [kin (cascade chunks p st (:tick ctx))]
        [[p new (cond-> [[:sound (sound-kind new false) 1.0 1.0]]
                  (seq kin) (conj [:schedule kin]))]]))))

(def rule
  {:name    :eyeblossom
   :match?  (fn [_chunks st _p] (eyeblossom? st))
   :wake    wake
   :reshape (:due support/rule)
   :due     switch-due})
