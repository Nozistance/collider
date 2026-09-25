(ns collider.world.blocks.fire
  "Fire: where it catches, how it spreads, and when it burns out."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.random :as random]
            [collider.vec :as v]
            [collider.world.block :as block]
            [collider.world.direction :as dir]
            [collider.world.chunk :as chunk]
            [collider.world.env.difficulty :as difficulty]
            [collider.world.env.biome :as biome]
            [collider.world.env.weather :as weather]
            [collider.world.blocks.support :as support]))

(set! *warn-on-reflection* true)

(def ^:const ages 16)

(defn fire-state?
  "Tells whether st is a fire block."
  [st]
  (block/fire? (long st)))

(defn fire-state
  "Returns the fire block of age."
  ^long [^long age]
  (block/state :fire {:age (keyword (str age))}))

(defn age
  "Returns the age of the fire block st."
  ^long [st]
  (block/prop-long (long st) :age))

(defn- with-age ^long [^long st ^long age]
  (let [props (assoc (block/props-of st) :age (keyword (str age)))]
    (block/state :fire props)))

(def ^:private side-offsets
  {:north [0 0 -1] :south [0 0 1] :west [-1 0 0] :east [1 0 0]
   :up [0 1 0]})

(defn- block-near ^long [chunks p d]
  (max 0 (long (chunk/at-void chunks (mapv + p d)))))

(defn- side-fire [chunks p]
  (into {:age :0}
        (map (fn [[k d]]
               [k (if (block/burnable? (block-near chunks p d))
                    :true
                    :false)]))
        side-offsets))

(defn state-for
  "Returns the fire block that fits at p."
  ^long [chunks p]
  (let [below (block-near chunks p [0 -1 0])]
    (cond
      (block/tagged? below "soul_fire_base_blocks")
      (block/state :soul-fire)
      (or (block/burnable? below) (block/face-sturdy? below :up))
      (fire-state 0)
      :else (block/state :fire (side-fire chunks p)))))

(defn- odds ^long [^long st k]
  (if (or (neg? st) (block/waterlogged? st))
    0
    (long (get-in (data/fire) [(block/block-of st) k] 0))))

(defn- can-burn? [^long st] (pos? (odds st :ignite)))

(defn- valid-location? [chunks p]
  (boolean (some (fn [d]
                   (can-burn? (chunk/at-void chunks (mapv + p d))))
                 dir/around)))

(defn- ignite-odds ^long [chunks p]
  (if (not (zero? (chunk/at-void chunks p)))
    0
    (reduce (fn [^long m d]
              (let [st (chunk/at-void chunks (mapv + p d))]
                (max m (odds st :ignite))))
            0 dir/around)))

(defn- pick ^long [roll salt ^long n]
  (long (Math/floor (* (double (roll salt)) n))))

(defn state-with-age
  "Returns the fire block for p, aged a when it is fire."
  ^long [chunks p ^long a]
  (let [st (state-for chunks p)]
    (if (fire-state? st) (with-age st a) st)))

(defn- spread-age ^long [roll salt ^long a]
  (min 15 (+ a (quot (pick roll salt 5) 4))))

(def ^:private rain-sides
  [[0 0 0] [-1 0 0] [1 0 0] [0 0 -1] [0 0 1]])

(defn- near-rain? [chunks ctx p]
  (boolean (some (fn [d]
                   (weather/raining-at? ctx chunks (mapv + p d)))
                 rain-sides)))

(defn- burn-out [chunks ctx p d chance roll a]
  (let [q (mapv + p d) st (chunk/at-void chunks q)]
    (when (< (pick roll [:burn q] chance) (odds st :burn))
      (if (and (< (pick roll [:burn-age q] (+ a 10)) 5)
               (not (weather/raining-at? ctx chunks q)))
        (let [aged (spread-age roll [:burn-spread q] a)]
          [q (state-with-age chunks q aged)])
        [q 0]))))

(def ^:private burn-sides
  [[[1 0 0] 300] [[-1 0 0] 300] [[0 -1 0] 250] [[0 1 0] 250]
   [[0 0 -1] 300] [[0 0 1] 300]])

(defn- burnout? [ctx p]
  (biome/increased-fire-burnout? (biome/at (:dim ctx) p)))

(def ^:private catch-offsets
  (for [xx [-1 0 1] zz [-1 0 1] yy (range -1 5)
        :when (not (and (zero? (long xx)) (zero? (long yy))
                        (zero? (long zz))))]
    [xx yy zz]))

(defn- catch-odds ^long [io a difficulty extra?]
  (let [o (quot (+ (long io) 40 (* (long difficulty) 7))
                (+ (long a) 30))]
    (if extra? (quot o 2) o)))

(defn- wet? [chunks ctx q]
  (and (weather/raining? ctx) (near-rain? chunks ctx q)))

(defn- catch-rate ^long [^long yy]
  (if (> yy 1) (+ 100 (* (dec yy) 100)) 100))

(defn- catch-at [chunks p ctx roll a difficulty extra? d]
  (let [rate (catch-rate (long (nth d 1)))
        q (mapv + p d)
        io (ignite-odds chunks q)
        o (catch-odds io a difficulty extra?)]
    (when (and (pos? io) (pos? o) (<= (pick roll [:catch q] rate) o)
               (not (wet? chunks ctx q)))
      (let [aged (spread-age roll [:catch-age q] a)]
        [q (state-with-age chunks q aged)]))))

(defn- catch-fire [chunks p ctx roll a difficulty]
  (let [extra? (burnout? ctx p)]
    (keep #(catch-at chunks p ctx roll a difficulty extra? %)
          catch-offsets)))

(defn- spread-changes [chunks p ctx roll a]
  (let [extra (if (burnout? ctx p) -50 0)
        out (fn [[d chance]]
              (let [c (+ (long chance) extra)]
                (burn-out chunks ctx p d c roll a)))]
    (concat (keep out burn-sides)
            (catch-fire chunks p ctx roll a (difficulty/id ctx)))))

(defn- far-sq ^double [q p]
  (let [dx (- (v/x q) (double (long (p 0))))
        dy (- (v/y q) (double (long (p 1))))
        dz (- (v/z q) (double (long (p 2))))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- near-player? [ctx p]
  (let [r (long (get-in ctx [:rules :fire-spread-radius-around-player]
                        128))
        near? (fn [q] (< (far-sq q p) (double (* r r))))]
    (or (= -1 r) (boolean (some near? (:players ctx))))))

(defn- aged-change [st a a' p]
  (when (not= a a') [[p (with-age st a')]]))

(defn- rained-out? [chunks p ctx r infiniburn? a]
  (and (not infiniburn?)
       (weather/raining? ctx)
       (near-rain? chunks ctx p)
       (< (double (r :rain-out)) (+ 0.2 (* (double a) 0.03)))))

(defn- unfed-changes [p below a aged]
  (if (or (neg? below) (not (block/face-sturdy? below :up)) (> a 3))
    [[p 0]]
    aged))

(defn- died-out? [r ^long a below]
  (and (= a 15) (< (pick r :out 4) 1) (not (can-burn? below))))

(defn- aged-of [st r p]
  (let [a (age st)]
    (aged-change st a (min 15 (+ a (quot (pick r :age 3) 2))) p)))

(defn- infiniburn-tag
  "Returns the tag of the blocks fire burns on forever in the
  level of ctx."
  [ctx]
  (let [dim (or (:dim ctx) :overworld)
        tag (:infiniburn (data/dimension-type dim))]
    (str/replace (name tag) "-" "_")))

(defn- tick-changes [chunks p ctx]
  (let [st (chunk/at-void chunks p)
        r (fn [salt] (random/of-key (:tick ctx) p salt))
        below (chunk/at-void chunks (mapv + p [0 -1 0]))
        forever? (block/tagged? (max 0 below) (infiniburn-tag ctx))
        a (age st)
        aged (aged-of st r p)
        spread #(concat aged (spread-changes chunks p ctx r a))]
    (cond
      (not (support/supported? chunks p st)) [[p 0]]
      (rained-out? chunks p ctx r forever? a) [[p 0]]
      forever? (spread)
      (not (valid-location? chunks p)) (unfed-changes p below a aged)
      (died-out? r a below) [[p 0]]
      :else (spread))))

(defn- fire-delay ^long [tick p]
  (+ (long tick) 30 (mod (long (hash [p tick])) 10)))

(defn- wake-at [chunks tick p _old _self?]
  (if (support/supported? chunks p (chunk/chunks-get-block chunks p))
    (fire-delay tick p)
    (inc (long tick))))

(def rule
  {:name   :fire
   :match? (fn [_chunks st _p] (fire-state? st))
   :wake   wake-at
   :again  (fn [_chunks tick p] (fire-delay tick p))
   :due    (fn [chunks p ctx]
             (when (near-player? ctx p)
               (tick-changes chunks p ctx)))})
