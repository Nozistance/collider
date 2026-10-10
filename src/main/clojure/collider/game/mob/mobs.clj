(ns collider.game.mob.mobs
  "Mob kinds and the start state of a new mob."
  (:require [collider.data :as data]
            [collider.game.entity.size :as size]
            [collider.game.mob.goat :as goat]
            [collider.game.mob.shell :as shell]
            [collider.game.mob.variant :as variant]
            [collider.num :as num]
            [collider.random :as random]
            [collider.world.space.path :as path]))

(set! *warn-on-reflection* true)

(defn- coat [registry]
  (fn [ks place] (variant/pick registry place ks)))

(def ^:private cow
  {:sounds         :cow
   :voices         [:classic :moody]
   :shared         #{:milk}
   :food           "cow_food"
   :spawns-on      "animals_spawnable_on"
   :spawn-look     (coat "cow_variant")})

(def ^:private water-walker
  (update path/cow :malus assoc :water 0.0))

(def ^:private mooshroom
  (-> (assoc cow :ground :mycelium :voices [:classic]
             :spawns-on "mooshrooms_spawnable_on")
      (dissoc :spawn-look)))

(def ^:private goat-walker
  (update path/cow :malus assoc :on-top-of-powder-snow -1.0))

(def types
  "The facts of each mob type."
  {:sheep     {:sounds        :sheep
               :food          "sheep_food"
               :spawns-on     "animals_spawnable_on"
               :spawn-look    variant/sheep-color}
   :cow       cow
   :mooshroom mooshroom
   :pig       {:sounds      :pig
               :baby-sounds :baby-pig
               :voices      [:classic :big :mini]
               :shared      #{:step}
               :eats-aloud? true
               :food        "pig_food"
               :spawns-on   "animals_spawnable_on"
               :spawn-look  (coat "pig_variant")}
   :chicken   {:sounds      :chicken
               :baby-sounds :baby-chicken
               :voices      [:classic :picky]
               :shared      #{:step}
               :food        "chicken_food"
               :fall-drag   0.6
               :walker      water-walker
               :spawns-on   "animals_spawnable_on"
               :spawn-look  (coat "chicken_variant")}
   :rabbit    {:sounds      :rabbit
               :block-steps? true
               :food        "rabbit_food"
               :spawns-on   "rabbits_spawnable_on"
               :spawn-look  variant/rabbit-variant}
   :armadillo {:sounds      :armadillo
               :sound-key   shell/sound-key
               :head-y-rot  shell/max-head-y-rot
               :body-held?  shell/scared?
               :hurt-taken  shell/taken
               :hurt-reaction shell/hurt
               :eats-aloud? true
               :food        "armadillo_food"
               :spawns-on   "armadillo_spawnable_on"}
   :goat      {:sounds      :goat
               :voice       goat/voice
               :shared      #{:step :horn-break}
               :head-y-rot  (constantly 15.0)
               :eats-aloud? true
               :eat-sounds  2
               :fall-reduction 10
               :food        "goat_food"
               :walker      goat-walker
               :spawned     goat/spawned
               :born        goat/born
               :spawns-on   "goats_spawnable_on"}})

(defn egg-type
  "Returns the mob kind spawn egg item hatches, or nil."
  [item]
  (let [t (get-in (data/items) [item :spawns])]
    (when (contains? types t) t)))

(defn attribute
  "Returns the base value of attribute k of mob kind type, or zero
  when the kind has none."
  ^double [type k]
  (double (get-in (data/attributes) [type k] 0.0)))

(defn max-health [type] (attribute type :max-health))

(defn speed
  "Returns the base movement speed of mob kind type."
  ^double [type]
  (attribute type :movement-speed))

(defn mob-type? [type] (contains? types type))

(defn- food-of [[t m]]
  (when-let [tag (:food m)]
    [t (set (data/tag-values "item" tag))]))

(def ^:private ^:table foods
  (delay (into {} (keep food-of) types)))

(defn food
  "Returns the set of items mob kind type eats and breeds on."
  [type] (@foods type))

(defn walker
  "Returns the path parameters of mob kind type."
  [type] (get-in types [type :walker] path/cow))

(defn step-height
  "Returns how high a mob of kind type climbs without jumping."
  ^double [type]
  (double (float (attribute type :step-height))))

(defn max-head-y-rot
  "Returns how far mob e turns its head from its body."
  ^double [e]
  (if-let [f (:head-y-rot (types (:type e)))] (f e) 75.0))

(defn body-held?
  "Returns true when mob e keeps its body from turning."
  [e]
  (if-let [f (:body-held? (types (:type e)))] (boolean (f e)) false))

(defn- voice-of [m e]
  (if-let [f (:voice m)]
    (f e)
    (get (:voices m) (long (or (:sound-variant e) 0)) :classic)))

(defn- voiced [s voice]
  (if (= :classic voice) s (keyword (str (name s) "-" (name voice)))))

(defn- sound-set [m e k]
  (cond (and (some? (:baby-until e)) (:baby-sounds m))
        (:baby-sounds m)
        (contains? (:shared m) k) (:sounds m)
        :else (voiced (:sounds m) (voice-of m e))))

(defn sound-of
  "Returns the sound mob e makes for k, such as :say or :hurt.
  Each voice of a breed has sounds of its own, and a baby may sound
  as a baby. A breed may change or hush a sound by its state."
  [e k]
  (let [m (types (:type e))
        k (if-let [f (:sound-key m)] (f e k) k)]
    (when (and k (:sounds m))
      (keyword (name (sound-set m e k)) (name k)))))

(defn eating-sound
  "Returns the sound mob e eats with, or nil for a breed that eats
  in silence."
  [e]
  (when (:eats-aloud? (types (:type e))) (sound-of e :eat)))

(def ^:private coat-keys
  {:cow [:cow-variant :cow-sound] :pig [:pig-variant :pig-sound]
   :chicken [:chicken-variant :chicken-sound]})

(defn burning? [e] (boolean (:burning? e)))

(defn- variant ^long [e] (long (or (:variant e) 0)))

(defn- coat-of [e] (or (:variant e) :temperate))

(defn- own-metadata [e]
  (let [type (:type e)]
    (case type
      :sheep (cond-> {:color (long (or (:color e) 0))}
               (:sheared? e) (assoc :sheared? true))
      (:cow :pig :chicken)
      (let [[ck vk] (coat-keys type)]
        {ck (coat-of e) vk (voice-of (types type) e)})
      (:mooshroom :rabbit) {:variant (variant e)}
      :armadillo {:armadillo-state (shell/state-id e)}
      :goat (goat/metadata e))))

(defn metadata
  "Returns what clients see of mob e besides its movement."
  [e]
  (let [health (:health e (max-health (:type e)))]
    (cond-> (assoc (own-metadata e)
                   :baby? (some? (:baby-until e)) :burning? (burning? e)
                   :health (num/f32 (double health)))
      (:age-locked? e) (assoc :age-locked? true)
      (:custom-name e) (assoc :custom-name (:custom-name e))
      (:custom-name-visible e) (assoc :custom-name-visible true))))

(defn look-key
  "Returns the key that holds how a mob of kind type looks. A sheep
  wears a wool colour, and any other kind a variant."
  [type]
  (if (= :sheep type) :color :variant))

(defn new-mob
  "Returns a fresh mob of kind type at pos, with nothing on its mind.
  It looks as look says, a wool colour or a variant."
  [type pos look tick]
  (cond-> {:type        type
           :pos         pos
           :vel         [0.0 0.0 0.0]
           :yaw         0.0 :pitch 0.0 :on-ground false
           (look-key type) look
           :task        nil
           :no-action   0
           :health      (max-health type)
           :health-sent (max-health type)
           :arrived     [(inc (* 2 (long tick))) nil]}
    (= :rabbit type) (assoc :hop {})))

(def ^:private ^:const follow-spread 0.11485000000000001)

(defn- follow-bonus
  "Returns the share of follow range that a new mob adds. The keys ks
  draw it on a triangle around zero."
  ^double [ks]
  (* follow-spread (- (random/of-key (conj ks :follow))
                      (random/of-key (conj ks :follow-2)))))

(defn egg-mob
  "Returns a mob hatched from a spawn egg at place.
  The keys ks decide its look, voice, yaw and follow range."
  [type pos ks tick place]
  (let [look-fn (get-in types [type :spawn-look] (fn [_ _] 0))
        voices (count (get-in types [type :voices] [:classic]))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)
        voice (long (* voices (random/of-key (conj ks :voice))))
        look (look-fn ks place)
        spawned (get-in types [type :spawned] (fn [e _ _] e))]
    (spawned (assoc (new-mob type pos look tick)
               :yaw yaw :head-yaw yaw :sound-variant voice
               :follow-bonus (follow-bonus ks))
             ks tick)))

(defn command-mob
  "Returns a mob summoned by a command at place. Its yaw and head
  yaw are drawn from 0 up to 2 pi, about 6.28 degrees. The keys ks
  decide its yaw, look and voice."
  [type pos ks tick place]
  (let [r (double (float (random/of-key (conj ks :yaw))))
        yaw (double (float (* r (double (float (* 2.0 Math/PI))))))]
    (assoc (egg-mob type pos ks tick place) :yaw yaw :head-yaw yaw)))

(def ^:const baby-start
  "The ticks a newborn takes to grow up."
  24000)

(defn natural-mob
  "Returns a mob that natural spawning puts at pos. Its body faces
  yaw and its head stays at zero. It is a baby when baby? says so.
  The keys ks decide its look, voice and follow range."
  [type pos ks tick place yaw baby?]
  (cond-> (assoc (egg-mob type pos ks tick place)
            :yaw (double yaw) :head-yaw 0.0)
    baby? (assoc :baby-until (+ (long tick) baby-start))))

(def ^:const ambient-interval
  "The ticks a mob keeps quiet after it was heard or hurt."
  120)

(defn in-love? [e t] (> (long (or (:love-until e) 0)) (long t)))

(defn baby? [e] (some? (:baby-until e)))

(def ^:const death-ticks 20)

(defn death-ends?
  "Returns true when dead mob e leaves at the start of this tick,
  before its step."
  [e]
  (and (not (pos? (double (:health e 1.0))))
       (>= (inc (long (or (:death-time e) 0))) death-ticks)))

(defn box-of
  [e]
  (size/box e))

(defn eye-height
  "Returns how far above its position mob e looks out."
  ^double [e]
  (size/eye e))

(def ^:private ^:const jump-boost (double (float 0.1)))

(defn jump-boost-power
  "Returns how much higher jump boost lifts mob e."
  ^double [e]
  (if-let [b (get (:effects e) :jump-boost)]
    (num/fmul jump-boost (double (float (inc (long (:amplifier b))))))
    0.0))

(def ^:private ^:const shallow-eye 0.4)

(defn fluid-jump-threshold
  "Returns how deep the fluid around mob e must be before it swims
  up. A mob with its eyes below 0.4 swims up in any fluid."
  ^double [e]
  (if (< (eye-height e) shallow-eye) 0.0 0.4))

(defn loot-entity
  "Returns mob e as the predicates of its loot table see it."
  [e]
  (let [shroom (if (= 1 (:variant e)) :brown :red)
        wool (long (or (:color e) 0))
        components (case (:type e)
                     :sheep {:sheep/color (variant/dye-colors wool)}
                     :mooshroom {:mooshroom/variant shroom}
                     :chicken {:chicken/variant (coat-of e)}
                     :pig {:pig/variant (coat-of e)}
                     nil)]
    (cond-> {:type (:type e) :baby? (baby? e)
           :sheared? (boolean (:sheared? e))}
      components (assoc :components components))))

(defn panicking? [e t] (< (long t) (long (or (:panic-until e) 0))))
