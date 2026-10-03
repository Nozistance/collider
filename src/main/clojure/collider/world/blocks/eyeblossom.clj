(ns collider.world.blocks.eyeblossom
  "Eyeblossoms, open by night and closed by day."
  (:require [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(defn- eyeblossom? [^long st] (= :eyeblossom (block/type-of st)))

(defn night?
  "Returns true when the time of day falls in the hours an eyeblossom
  stays open."
  [^long time]
  (<= 12600 (mod time 24000) 23400))

(defn switched
  "Returns the eyeblossom st opened or closed for the time of day.
  Returns nil when it already fits."
  [^long st ^long time]
  (let [open? (= :open-eyeblossom (block/block-of st))
        night (night? time)]
    (when (not= open? night)
      (block/state (if night :open-eyeblossom :closed-eyeblossom)))))

(defn- sound-kind [^long st long-sound?]
  (let [open? (= :open-eyeblossom (block/block-of st))]
    (if long-sound?
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
        r (random/of-key tick q :eyeblossom)]
    (+ tick (random/between r lo hi))))

(defn- cascade [chunks p ^long old ^long tick]
  (reduce (fn [m q]
            (if (not= old (chunk/at chunks q))
              m
              (update m (follow-tick p q tick) (fnil conj [])
                      (chunk/block-pos->id q))))
          {} (neighbours p)))

(defn- wake [chunks _dim _tick p _old _side]
  (let [st (chunk/at chunks p)]
    (when-not (support/supported? chunks p st) :neighbor)))

(def ^:private colors
  {:open-eyeblossom 16545810 :closed-eyeblossom 6250335})

(defn- trail
  [[x y z :as p] ^long new ^long tick]
  (let [r #(double (random/of-key tick p :eyeblossom-trail %))
        life (+ 0.5 (r 0))
        v [(- (r 1) 0.5) (+ (r 2) 1.0) (- (r 3) 0.5)]
        target (mapv (fn [c d] (+ (double c) 0.5 (* (double d) life)))
                     [x y z] v)]
    [:trail target (colors (block/block-of new))
     (long (* 20.0 life))]))

(defn switch-fx
  "Returns the effects of the eyeblossom st at p turning to new on
  tick. The eyeblossoms near it follow later, and long-sound? picks
  the longer sound."
  [chunks p st new tick long-sound?]
  (let [kin (cascade chunks p st tick)]
    (cond-> [(trail p new tick)
             [:sound (sound-kind new long-sound?) 1.0 1.0]]
      (seq kin) (conj [:schedule kin]))))

(defn- switch-due [chunks p ctx]
  (let [st (chunk/at chunks p)
        time (long (:time-of-day ctx 0))]
    (when-let [new (switched st time)]
      [[p new (switch-fx chunks p st new (:tick ctx) false)]])))

(def rule
  "The block rule that breaks eyeblossoms without support and turns
  them with the time of day."
  {:name    :eyeblossom
   :match?  (fn [_chunks st _p] (eyeblossom? st))
   :wake    wake
   :reach   3
   :reshape (:due support/rule)
   :due     switch-due})
