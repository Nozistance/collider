(ns collider.world.blocks.fire
  "Fire, where it catches, how it spreads and when it burns out."
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.world.env.dimension :as dimension]
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

(defn fire-state
  "Returns a fire of the given age that clings to no side."
  ^long [^long age]
  (block/state :fire {:age (keyword (str age))}))

(defn age
  "Returns how far the fire st has burnt, from 0 to 15."
  ^long [st]
  (block/prop-long (long st) :age))

(defn- with-age ^long [^long st ^long age]
  (let [props (assoc (block/props-of st) :age (keyword (str age)))]
    (block/state :fire props)))

(def ^:private side-offsets
  (select-keys dir/offset [:north :south :west :east :up]))

(defn- block-near ^long [chunks p d]
  (max 0 (long (chunk/at-void chunks (mapv + p d)))))

(defn- side-fire [chunks p]
  (into {:age :0}
        (map (fn [[k d]]
               (let [st (block-near chunks p d)]
                 [k (block/flag (block/burnable? st))])))
        side-offsets))

(defn state-for
  "Returns the fire block that fits at p."
  ^long [chunks p]
  (let [below (block-near chunks p (dir/offset :down))]
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

(defn state-with-age
  "Returns the fire block for p, aged a when it is fire."
  ^long [chunks p ^long a]
  (let [st (state-for chunks p)]
    (if (block/fire? st) (with-age st a) st)))

(defn- spread-age ^long [roll salt ^long a]
  (min 15 (+ a (quot (random/below (roll salt) 5) 4))))

(def ^:private rain-sides
  (into [[0 0 0]] (map dir/offset) [:west :east :north :south]))

(defn- near-rain? [chunks ctx p]
  (boolean (some (fn [d]
                   (weather/raining-at? ctx chunks (mapv + p d)))
                 rain-sides)))

(defn- burnt
  [[q st'] ^long st]
  (if (block/tnt? st) [q st' [:prime]] [q st']))

(defn- burnt-to [chunks ctx q roll a]
  (if (and (< (random/below (roll [:burn-age q]) (+ a 10)) 5)
           (not (weather/raining-at? ctx chunks q)))
    [q (state-with-age chunks q (spread-age roll [:burn-spread q] a))]
    [q 0]))

(defn- burn-out [chunks ctx p d chance roll a]
  (let [q (mapv + p d) st (chunk/at-void chunks q)]
    (when (< (random/below (roll [:burn q]) chance) (odds st :burn))
      (burnt (burnt-to chunks ctx q roll a) st))))

(def ^:private burn-sides
  (mapv (fn [[d chance]] [(dir/offset d) chance])
        [[:east 300] [:west 300] [:down 250] [:up 250]
         [:north 300] [:south 300]]))

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

(defn- catch-rate ^long [^long yy] (* 100 (max 1 yy)))

(defn- catch-at [chunks p ctx roll a difficulty extra? d]
  (let [rate (catch-rate (long (nth d 1)))
        q (mapv + p d)
        io (ignite-odds chunks q)
        o (catch-odds io a difficulty extra?)]
    (when (and (pos? io) (pos? o)
               (<= (random/below (roll [:catch q]) rate) o)
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
  (let [k :fire-spread-radius-around-player
        r (long (get-in ctx [:rules k] 128))
        near? (fn [q] (< (far-sq q p) (double (* r r))))]
    (or (= -1 r) (boolean (some near? (:players ctx))))))

(defn- aged-change [st a a' p]
  (when (not= a a') [[p (with-age st a')]]))

(defn- rained-out? [chunks p ctx r infiniburn? a]
  (and (not infiniburn?)
       (wet? chunks ctx p)
       (< (double (r :rain-out)) (+ 0.2 (* (double a) 0.03)))))

(defn- unfed-changes [p below a aged]
  (if (or (neg? below) (not (block/face-sturdy? below :up)) (> a 3))
    [[p 0]]
    aged))

(defn- died-out? [r ^long a below]
  (and (= a 15) (< (random/below (r :out) 4) 1)
       (not (can-burn? below))))

(defn- aged-of [st r p]
  (let [a (age st)
        a' (min 15 (+ a (quot (random/below (r :age) 3) 2)))]
    (aged-change st a a' p)))

(defn- infiniburn-tag [ctx]
  (let [dim (or (:dim ctx) :overworld)
        tag (:infiniburn (dimension/type-of dim))]
    (str/replace (name tag) "-" "_")))

(defn- tick-changes [chunks p ctx]
  (let [st (chunk/at-void chunks p)
        r (fn [salt] (random/of-key (:tick ctx) p salt))
        below (chunk/at-void chunks (dir/down p))
        infiniburn? (block/tagged? (max 0 below) (infiniburn-tag ctx))
        a (age st)
        aged (aged-of st r p)
        spread #(concat aged (spread-changes chunks p ctx r a))]
    (cond
      (not (support/supported? chunks p st)) [[p 0]]
      (rained-out? chunks p ctx r infiniburn? a) [[p 0]]
      infiniburn? (spread)
      (not (valid-location? chunks p)) (unfed-changes p below a aged)
      (died-out? r a below) [[p 0]]
      :else (spread))))

(defn- fire-delay ^long [tick p]
  (+ (long tick) 30 (mod (long (hash [p tick])) 10)))

(defn- wake-at [_chunks _dim tick p _old side]
  (when (nil? side) (fire-delay tick p)))

(def rule
  "The block rule that ages fire, spreads it, burns blocks away and
  puts it out."
  {:name   :fire
   :match? (fn [_chunks st _p] (block/fire? st))
   :wake   wake-at
   :again  (fn [_chunks tick p] (fire-delay tick p))
   :reach  2
   :lit?   (fn [_st ctx] (weather/raining? ctx))
   :due    (fn [chunks p ctx]
             (when (near-player? ctx p)
               (tick-changes chunks p ctx)))})
