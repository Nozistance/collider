(ns collider.world.fire
  "Fire: survival and ageing as in FireBlock 26.2. Spread to neighbors
   (checkBurnOut, tryCatchFire) and rain are not done yet."
  (:require [collider.rnd :as rnd]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.support :as support]))

(set! *warn-on-reflection* true)

(def ^:const ages 16)
(defn fire-state? [st] (block/fire? (long st)))
(defn fire-state ^long [^long age] (block/state :fire {:age (keyword (str age))}))
(defn age ^long [st] (long (Long/parseLong (name (:age (block/props-of (long st)))))))

(defn- with-age ^long [^long st ^long age]
  (block/state :fire (assoc (block/props-of st) :age (keyword (str age)))))

(def ^:private around6 [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- state-at ^long [chunks [_ y _ :as p]]
  (if (chunk/in-range? (long y)) (chunk/chunks-get-block chunks gen/flat-chunk p) -1))

(defn- valid-location?
  "FireBlock.isValidFireLocation: a burnable block is adjacent."
  [chunks p]
  (boolean (some (fn [d] (let [n (state-at chunks (mapv + p d))] (and (pos? n) (block/burnable? n)))) around6)))

(defn- tick-changes
  "FireBlock.tick without spread. Remove fire that cannot survive. Age grows
   by nextInt(3)/2. With no burnable neighbor, fire goes out when the block
   below is not solid or age > 3. At age 15, chance 1/4 to go out above a
   non-burnable block."
  [chunks p ctx]
  (let [st (state-at chunks p)
        r (fn [salt] (rnd/rnd [(:tick ctx) p salt]))
        below (state-at chunks (mapv + p [0 -1 0]))
        a (age st)
        a' (min 15 (+ a (quot (long (Math/floor (* 3.0 (r :age)))) 2)))]
    (cond
      (not (support/supported? chunks gen/flat-chunk p st)) [[p 0]]
      (not (valid-location? chunks p))
      (if (or (neg? below) (not (block/face-sturdy? below :up)) (> a 3))
        [[p 0]]
        (when (not= a a') [[p (with-age st a')]]))
      (and (= a 15) (< (r :out) 0.25) (not (and (pos? below) (block/burnable? below)))) [[p 0]]
      (not= a a') [[p (with-age st a')]])))

(defn- fire-delay ^long [tick p] (+ (long tick) 30 (mod (long (hash [p tick])) 10)))

(def rule
  {:name   :fire
   :match? (fn [_chunks st _p] (fire-state? st))
   ;; getFireTickDelay: 30 + nextInt(10). Without support: at once (updateShape gives air)
   :wake   (fn [chunks tick p _old _self?]
             (if (support/supported? chunks gen/flat-chunk p (chunk/chunks-get-block chunks gen/flat-chunk p))
               (fire-delay tick p)
               (inc (long tick))))
   :again  (fn [_chunks tick p] (fire-delay tick p))
   :due    (fn [chunks p ctx] (tick-changes chunks p ctx))})
