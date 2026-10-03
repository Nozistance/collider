(ns collider.game.mob.mobs
  "Mob kinds and the start state of a new mob."
  (:require [collider.data :as data]
            [collider.game.entity.size :as size]
            [collider.random :as random]
            [collider.world.env.biome :as biome]
            [collider.world.space.path :as path]))

(set! *warn-on-reflection* true)

(def dye-colors
  "The dye colours by their id, as the loot tables name them."
  [:white :orange :magenta :light-blue :yellow :lime :pink :gray
   :light-gray :cyan :purple :blue :brown :green :red :black])

(def ^:private color-ids
  (into {} (map-indexed (fn [i c] [c i])) dye-colors))

(defn color-id
  "Returns the id of dye colour c, or nil when c is no dye colour."
  [c] (color-ids c))

(def ^:private spawn-configs
  {:temperate
   {:rare [[5 :black] [5 :gray] [5 :light-gray] [3 :brown]]
    :common :white}
   :warm
   {:rare [[5 :gray] [5 :light-gray] [5 :white] [3 :black]]
    :common :brown}
   :cold
   {:rare [[5 :light-gray] [5 :gray] [5 :white] [3 :brown]]
    :common :black}})

(defn- biome-tag [tag]
  (delay (set (data/tag-values "worldgen/biome" tag))))

(def ^:private ^:table warm-biomes
  (biome-tag "spawns_warm_variant_farm_animals"))

(def ^:private ^:table cold-biomes
  (biome-tag "spawns_cold_variant_farm_animals"))

(defn- biome-kind [biome]
  (let [n (:name biome)]
    (cond (@warm-biomes n) :warm
          (@cold-biomes n) :cold
          :else :temperate)))

(defn- spawn-config [biome] (spawn-configs (biome-kind biome)))

(def coats
  "The coats of the farm animals that come in a warm and a cold
  variant, by their ids."
  [:temperate :warm :cold])

(def ^:private coat-ids
  {:temperate 0 :warm 1 :cold 2})

(defn- coat [_ biome] (coat-ids (biome-kind biome)))

(def ^:private ^:const rare-total 100.0)

(def ^:private ^:const common-total 500.0)

(def ^:private ^:const common-weight 499)

(defn- weighted [^long r entries]
  (loop [lo 0 [[w c] & more] entries]
    (when w
      (if (< r (+ lo (long w))) c (recur (+ lo (long w)) more)))))

(defn- common-color [ks common]
  (if (< (long (* common-total (random/of-key (conj ks :pink))))
         common-weight)
    common
    :pink))

(defn- sheep-color [ks biome]
  (let [{:keys [rare common]} (spawn-config biome)
        r (long (* rare-total (random/of-key ks)))]
    (color-id (or (weighted r rare) (common-color ks common)))))

(def ^:private ^:table white-rabbit-biomes
  (biome-tag "spawns_white_rabbits"))

(def ^:private ^:table gold-rabbit-biomes
  (biome-tag "spawns_gold_rabbits"))

(def rabbit-variants
  {0 :brown 1 :white 2 :black 3 :white-splotched 4 :gold 5 :salt
   99 :evil})

(defn- mixed-rabbit ^long [^long r]
  (cond (< r 50) 0 (< r 90) 5 :else 2))

(defn rabbit-variant
  "Returns the variant a rabbit takes in biome by the keys ks."
  [ks biome]
  (let [n (:name biome)
        r (long (* 100.0 (random/of-key (conj ks :rabbit))))]
    (cond (@white-rabbit-biomes n) (if (< r 80) 1 3)
          (@gold-rabbit-biomes n) 4
          :else (mixed-rabbit r))))

(def ^:private cow
  {:sounds         :cow
   :voices         [:classic :moody]
   :food           "cow_food"
   :spawns-on      "animals_spawnable_on"
   :spawn-color    coat})

(def ^:private water-walker
  (update path/cow :malus assoc :water 0.0))

(def ^:private mooshroom
  (-> (assoc cow :ground :mycelium :voices [:classic]
             :spawns-on "mooshrooms_spawnable_on")
      (dissoc :spawn-color)))

(def types
  "The facts of each mob type."
  {:sheep     {:sounds        :sheep
               :food          "sheep_food"
               :spawns-on     "animals_spawnable_on"
               :spawn-color   sheep-color}
   :cow       cow
   :mooshroom mooshroom
   :pig       {:sounds      :pig
               :baby-sounds :baby-pig
               :voices      [:classic :big :mini]
               :shared      #{:step}
               :eats-aloud? true
               :food        "pig_food"
               :spawns-on   "animals_spawnable_on"
               :spawn-color coat}
   :chicken   {:sounds      :chicken
               :baby-sounds :baby-chicken
               :voices      [:classic :picky]
               :shared      #{:step}
               :food        "chicken_food"
               :fall-drag   0.6
               :walker      water-walker
               :spawns-on   "animals_spawnable_on"
               :spawn-color coat}
   :rabbit    {:sounds      :rabbit
               :block-steps? true
               :food        "rabbit_food"
               :spawns-on   "rabbits_spawnable_on"
               :spawn-color rabbit-variant}})

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

(defn- voice-of [m e]
  (get (:voices m) (long (or (:sound-variant e) 0)) :classic))

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
  as a baby."
  [e k]
  (let [m (types (:type e))]
    (when (:sounds m)
      (keyword (name (sound-set m e k)) (name k)))))

(def ^:private sheep-metas
  (vec (for [color (range 16) baby [false true]
             burning [false true] sheared [false true]]
         (cond-> {:color color :baby? baby :burning? burning}
           sheared (assoc :sheared? true)))))

(defn- bit ^long [b ^long v] (if b v 0))

(defn- sheep-meta [e]
  (let [c (long (or (:color e) 0))]
    (when (and (<= 0 c) (< c 16))
      (nth sheep-metas
           (bit-or (bit-shift-left c 3)
                   (bit (some? (:baby-until e)) 4)
                   (bit (:burning? e) 2) (bit (:sheared? e) 1))))))

(defn eating-sound
  "Returns the sound mob e eats with, or nil for a breed that eats
  in silence."
  [e]
  (when (:eats-aloud? (types (:type e))) (sound-of e :eat)))

(defn- coat-metas [type ck vk]
  (into {} (for [c coats v (:voices (types type))
                 baby [false true] burning [false true]]
             [[c v baby burning]
              {ck c vk v :baby? baby :burning? burning}])))

(def ^:private coat-meta
  {:cow (coat-metas :cow :cow-variant :cow-sound)
   :pig (coat-metas :pig :pig-variant :pig-sound)
   :chicken (coat-metas :chicken :chicken-variant :chicken-sound)})

(def ^:private mooshroom-meta
  (into {} (for [v [0 1] baby [false true] burning [false true]]
             [[v baby burning]
              {:variant v :baby? baby :burning? burning}])))

(def ^:private rabbit-metas
  (into {} (for [v (keys rabbit-variants) baby [false true]
                 burning [false true]]
             [[v baby burning]
              {:variant v :baby? baby :burning? burning}])))

(defn burning? [e] (boolean (:burning? e)))

(defn metadata
  "Returns what clients see of mob e besides its movement."
  [e]
  (case (:type e)
    :sheep (sheep-meta e)
    (:cow :pig :chicken)
    ((coat-meta (:type e))
     [(coats (long (or (:color e) 0))) (voice-of (types (:type e)) e)
      (some? (:baby-until e)) (burning? e)])
    :mooshroom (mooshroom-meta
                [(long (or (:color e) 0))
                 (some? (:baby-until e)) (burning? e)])
    :rabbit (rabbit-metas
             [(long (or (:color e) 0))
              (some? (:baby-until e)) (burning? e)])))

(defn new-mob
  "Returns a fresh mob of kind type at pos, with nothing on its mind."
  [type pos color tick]
  (cond-> {:type        type
           :pos         pos
           :vel         [0.0 0.0 0.0]
           :yaw         0.0 :pitch 0.0 :on-ground false
           :color       color
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
  "Returns a mob hatched from a spawn egg in level dim.
  The keys ks decide its colour, voice, yaw and follow range."
  [type pos ks tick dim]
  (let [color-fn (get-in types [type :spawn-color] (fn [_ _] 0))
        voices (count (get-in types [type :voices] [:classic]))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)
        voice (long (* voices (random/of-key (conj ks :voice))))
        color (color-fn ks (biome/at dim pos))]
    (assoc (new-mob type pos color tick)
      :yaw yaw :head-yaw yaw :sound-variant voice
      :follow-bonus (follow-bonus ks))))

(defn command-mob
  "Returns a mob summoned by a command in level dim. Its yaw and head
  yaw are drawn from 0 up to 2 pi, about 6.28 degrees. The keys ks
  decide its yaw, colour and voice."
  [type pos ks tick dim]
  (let [r (double (float (random/of-key (conj ks :yaw))))
        yaw (double (float (* r (double (float (* 2.0 Math/PI))))))]
    (assoc (egg-mob type pos ks tick dim) :yaw yaw :head-yaw yaw)))

(def ^:private ^:const baby-start 24000)

(defn natural-mob
  "Returns a mob that natural spawning puts at pos in level dim. Its
  body faces yaw and its head stays at zero. It is a baby when baby?
  says so. The keys ks decide its colour, voice and follow range."
  [type pos ks tick dim yaw baby?]
  (cond-> (assoc (egg-mob type pos ks tick dim)
            :yaw (double yaw) :head-yaw 0.0)
    baby? (assoc :baby-until (+ (long tick) baby-start))))

(defn exp-delay
  "Returns a wait of at least one tick, drawn from an exponential law
  with the given mean."
  ^long [mean ^long t ^long eid kind]
  (let [r (max 1.0E-9 (random/of-longs t eid (hash kind)))]
    (max 1 (long (* (double mean) (- (Math/log r)))))))

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

(def ^:const legacy-fluid-eye
  "The share of its height at which a mob tests the depth of the fluid
  it jumps in, in place of its real eye."
  0.85)

(def ^:const legacy-look-eye
  "The share of its height at which a mob looks and is looked at, in
  place of its real eye."
  0.95)

(def ^:const legacy-player-eye
  "The height above its feet at which a mob sees the eye of a player,
  in place of the eye of its pose."
  1.62)

(defn loot-entity
  "Returns mob e as the predicates of its loot table see it."
  [e]
  (let [n (long (or (:color e) 0))
        shroom (if (= 1 n) :brown :red)
        components (case (:type e)
                     :sheep {:sheep/color (dye-colors n)}
                     :mooshroom {:mooshroom/variant shroom}
                     :chicken {:chicken/variant (coats n)}
                     :pig {:pig/variant (coats n)}
                     nil)]
    (cond-> {:type (:type e) :baby? (baby? e)
           :sheared? (boolean (:sheared? e))}
      components (assoc :components components))))

(defn panicking? [e t] (< (long t) (long (or (:panic-until e) 0))))
