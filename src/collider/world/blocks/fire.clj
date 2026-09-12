(ns collider.world.blocks.fire
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.gen :as gen]
            [collider.world.env.biome :as biome]
            [collider.world.env.weather :as weather]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def ^:const ages 16)
(defn fire-state? [st] (block/fire? (long st)))
(defn fire-state ^long [^long age] (block/state :fire {:age (keyword (str age))}))
(defn age ^long [st] (block/prop-long (long st) :age))
(defn- with-age ^long [^long st ^long age]
  (block/state :fire (assoc (block/props-of st) :age (keyword (str age)))))

(def ^:private side-offsets
  {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0] :up [0 1 0]})
(defn state-for ^long [chunks p]
  (let [below (max 0 (long (gen/at-void chunks (mapv + p [0 -1 0]))))]
    (cond
      (block/tagged? below "soul_fire_base_blocks")
      (block/state :soul-fire)
      (or (block/burnable? below) (block/face-sturdy? below :up))
      (fire-state 0)
      :else
      (block/state :fire (into {:age :0}
                               (map (fn [[k d]]
                                      [k (if (block/burnable? (max 0 (long (gen/at-void chunks (mapv + p d)))))
                                           :true :false)]))
                               side-offsets)))))

(defn- odds ^long [^long st k]
  (if (or (neg? st) (block/waterlogged? st))
    0
    (long (get-in @data/fire [(block/block-of st) k] 0))))

(defn- can-burn? [^long st] (pos? (odds st :ignite)))

(defn- valid-location? [chunks p]
  (boolean (some (fn [d] (can-burn? (gen/at-void chunks (mapv + p d)))) dir/around)))

(defn- ignite-odds ^long [chunks p]
  (if (not (zero? (gen/at-void chunks p)))
    0
    (reduce (fn [^long m d] (max m (odds (gen/at-void chunks (mapv + p d)) :ignite))) 0 dir/around)))

(defn- pick ^long [roll salt ^long n] (long (Math/floor (* (double (roll salt)) n))))

(defn state-with-age ^long [chunks p ^long a]
  (let [st (state-for chunks p)]
    (if (fire-state? st) (with-age st a) st)))

(defn- spread-age ^long [roll salt ^long a] (min 15 (+ a (quot (pick roll salt 5) 4))))

(def ^:private rain-sides [[0 0 0] [-1 0 0] [1 0 0] [0 0 -1] [0 0 1]])

(defn- near-rain? [chunks ctx p]
  (boolean (some (fn [d] (weather/raining-at? ctx chunks (mapv + p d))) rain-sides)))

(defn- burn-out [chunks ctx p d chance roll a]
  (let [q (mapv + p d) st (gen/at-void chunks q)]
    (when (< (pick roll [:burn q] chance) (odds st :burn))
      (if (and (< (pick roll [:burn-age q] (+ a 10)) 5) (not (weather/raining-at? ctx chunks q)))
        [q (state-with-age chunks q (spread-age roll [:burn-spread q] a))]
        [q 0]))))

(def ^:private burn-sides
  [[[1 0 0] 300] [[-1 0 0] 300] [[0 -1 0] 250] [[0 1 0] 250] [[0 0 -1] 300] [[0 0 1] 300]])

(defn- burnout? [chunks p]
  (biome/increased-fire-burnout? (biome/at chunks p)))

(defn- catch-fire [chunks p ctx roll a difficulty]
  (let [extra? (burnout? chunks p)]
    (for [xx [-1 0 1] zz [-1 0 1] yy (range -1 5)
          :when (not (and (zero? (long xx)) (zero? (long yy)) (zero? (long zz))))
          :let [rate (if (> (long yy) 1) (+ 100 (* (dec (long yy)) 100)) 100)
                q (mapv + p [xx yy zz])
                io (ignite-odds chunks q)
                o (quot (+ io 40 (* (long difficulty) 7)) (+ a 30))
                o (if extra? (quot (long o) 2) o)]
          :when (and (pos? io) (pos? o) (<= (pick roll [:catch q] rate) o)
                     (not (and (weather/raining? ctx) (near-rain? chunks ctx q))))]
      [q (state-with-age chunks q (spread-age roll [:catch-age q] a))])))

(defn- spread-changes [chunks p ctx roll a]
  (let [extra (if (burnout? chunks p) -50 0)]
    (concat (keep (fn [[d chance]] (burn-out chunks ctx p d (+ (long chance) extra) roll a)) burn-sides)
            (catch-fire chunks p ctx roll a (difficulty/id ctx)))))

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
  (let [st (gen/at-void chunks p)
        r (fn [salt] (random/of-key [(:tick ctx) p salt]))
        below (gen/at-void chunks (mapv + p [0 -1 0]))
        a (age st)
        a' (min 15 (+ a (quot (pick r :age 3) 2)))
        aged (aged-change st a a' p)]
    (cond
      (not (support/supported? chunks gen/flat-chunk p st)) [[p 0]]
      (and (not (block/tagged? (max 0 below) "infiniburn_overworld"))
           (weather/raining? ctx)
           (near-rain? chunks ctx p)
           (< (double (r :rain-out)) (+ 0.2 (* (double a) 0.03)))) [[p 0]]
      (block/tagged? (max 0 below) "infiniburn_overworld") (concat aged (spread-changes chunks p ctx r a))
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
