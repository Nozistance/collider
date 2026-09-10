(ns collider.world.fire
  (:require [collider.data :as data]
            [collider.rnd :as rnd]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]
            [collider.world.difficulty :as difficulty]
            [collider.world.gen :as gen]
            [collider.world.weather :as weather]
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

(def ^:private soul-base #{:soul-sand :soul-soil})
(def ^:private side-offsets
  {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0] :up [0 1 0]})
(defn state-for ^long [chunks p]
  (let [below (max 0 (long (state-at chunks (mapv + p [0 -1 0]))))]
    (cond
      (contains? soul-base (block/block-of below))
      (block/state :soul-fire)
      (or (block/burnable? below) (block/face-sturdy? below :up))
      (fire-state 0)
      :else
      (block/state :fire (into {:age :0}
                               (map (fn [[k d]]
                                      [k (if (block/burnable? (max 0 (long (state-at chunks (mapv + p d)))))
                                           :true :false)]))
                               side-offsets)))))

(defn- odds ^long [^long st k]
  (if (or (neg? st) (block/waterlogged? st))
    0
    (long (get-in @data/fire [(block/block-of st) k] 0))))

(defn- can-burn? [^long st] (pos? (odds st :ignite)))

(defn- valid-location? [chunks p]
  (boolean (some (fn [d] (can-burn? (state-at chunks (mapv + p d)))) around6)))

(defn- ignite-odds ^long [chunks p]
  (if (not (zero? (state-at chunks p)))
    0
    (reduce (fn [^long m d] (max m (odds (state-at chunks (mapv + p d)) :ignite))) 0 around6)))

(defn- pick ^long [rnd salt ^long n] (long (Math/floor (* (double (rnd salt)) n))))

(defn state-with-age ^long [chunks p ^long a]
  (let [st (state-for chunks p)]
    (if (fire-state? st) (with-age st a) st)))

(defn- spread-age ^long [rnd salt ^long a] (min 15 (+ a (quot (pick rnd salt 5) 4))))

(def ^:private rain-sides [[0 0 0] [-1 0 0] [1 0 0] [0 0 -1] [0 0 1]])

(defn- near-rain? [chunks ctx p]
  (and (weather/raining? ctx)
       (boolean (some (fn [d] (weather/raining-at? chunks (mapv + p d))) rain-sides))))

(defn- burn-out [chunks p d chance rnd a]
  (let [q (mapv + p d) st (state-at chunks q)]
    (when (< (pick rnd [:burn q] chance) (odds st :burn))
      (if (and (< (pick rnd [:burn-age q] (+ a 10)) 5) (not (weather/raining-at? chunks q)))
        [q (state-with-age chunks q (spread-age rnd [:burn-spread q] a))]
        [q 0]))))

(def ^:private burn-sides
  [[[1 0 0] 300] [[-1 0 0] 300] [[0 -1 0] 250] [[0 1 0] 250] [[0 0 -1] 300] [[0 0 1] 300]])

(defn- catch-fire [chunks p ctx rnd a difficulty]
  (for [xx [-1 0 1] zz [-1 0 1] yy (range -1 5)
        :when (not (and (zero? (long xx)) (zero? (long yy)) (zero? (long zz))))
        :let [rate (if (> (long yy) 1) (+ 100 (* (dec (long yy)) 100)) 100)
              q (mapv + p [xx yy zz])
              io (ignite-odds chunks q)
              o (quot (+ io 40 (* (long difficulty) 7)) (+ a 30))]
        :when (and (pos? io) (pos? o) (<= (pick rnd [:catch q] rate) o)
                   (not (near-rain? chunks ctx q)))]
    [q (state-with-age chunks q (spread-age rnd [:catch-age q] a))]))

(defn- spread-changes [chunks p ctx rnd a]
  (concat (keep (fn [[d chance]] (burn-out chunks p d chance rnd a)) burn-sides)
          (catch-fire chunks p ctx rnd a (difficulty/id ctx))))

(def ^:private infiniburn #{:netherrack :magma-block})

(defn- far-sq ^double [q p]
  (let [dx (- (v/x q) (double (long (p 0))))
        dy (- (v/y q) (double (long (p 1))))
        dz (- (v/z q) (double (long (p 2))))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- near-player? [ctx p]
  (let [r (long (get-in ctx [:rules :fire-spread-radius-around-player] 128))]
    (or (= -1 r)
        (boolean (some (fn [q] (< (far-sq q p) (double (* r r)))) (:players ctx))))))

(defn- aged-change [st a a' p]
  (when (not= a a') [[p (with-age st a')]]))

(defn- tick-changes [chunks p ctx]
  (let [st (state-at chunks p)
        r (fn [salt] (rnd/rnd [(:tick ctx) p salt]))
        below (state-at chunks (mapv + p [0 -1 0]))
        a (age st)
        a' (min 15 (+ a (quot (pick r :age 3) 2)))
        aged (aged-change st a a' p)]
    (cond
      (not (support/supported? chunks gen/flat-chunk p st)) [[p 0]]
      (contains? infiniburn (block/block-of (max 0 below))) (concat aged (spread-changes chunks p ctx r a))
      (not (valid-location? chunks p))
      (if (or (neg? below) (not (block/face-sturdy? below :up)) (> a 3)) [[p 0]] aged)
      (and (= a 15) (< (pick r :out 4) 1) (not (can-burn? below))) [[p 0]]
      :else (concat aged (spread-changes chunks p ctx r a)))))

(defn- fire-delay ^long [tick p] (+ (long tick) 30 (mod (long (hash [p tick])) 10)))
(def rule
  {:name   :fire
   :match? (fn [_chunks st _p] (fire-state? st))
   :wake   (fn [chunks tick p _old _self?]
             (if (support/supported? chunks gen/flat-chunk p (chunk/chunks-get-block chunks gen/flat-chunk p))
               (fire-delay tick p)
               (inc (long tick))))
   :again  (fn [_chunks tick p] (fire-delay tick p))
   :due    (fn [chunks p ctx] (when (near-player? ctx p) (tick-changes chunks p ctx)))})
