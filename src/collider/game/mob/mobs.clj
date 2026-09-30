(ns collider.game.mob.mobs
  "Mob kinds and the start state of a new mob."
  (:require [collider.data :as data]
            [collider.random :as random]
            [collider.world.env.biome :as biome]))

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

(def cow-variants
  "The cow variants by their ids."
  [:temperate :warm :cold])

(def ^:private cow-variant-ids
  {:temperate 0 :warm 1 :cold 2})

(defn- cow-variant [_ biome] (cow-variant-ids (biome-kind biome)))

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

(defn- attr
  ^double [^double v] (double (float v)))

(def ^:private cow
  {:half           (attr 0.45) :height (attr 1.4)
   :eye            (attr 1.3) :baby-eye (attr 0.69)
   :speed          (attr 0.2)
   :max-health     10.0
   :sounds         :cow
   :sound-variants 2
   :breeding-item  :wheat
   :spawn-color    cow-variant})

(def types
  {:sheep     {:half          (attr 0.45)
               :height        (attr 1.3)
               :eye           (attr 1.235)
               :baby-eye      (attr 0.65625)
               :speed         (attr 0.23)
               :max-health    8.0
               :sounds        :sheep
               :breeding-item :wheat
               :spawn-color   sheep-color}
   :cow       cow
   :mooshroom (-> (assoc cow :ground :mycelium :sound-variants 1)
                  (dissoc :spawn-color))})

(defn egg-type
  "Returns the mob kind spawn egg item hatches, or nil."
  [item]
  (let [t (get-in (data/items) [item :spawns])]
    (when (contains? types t) t)))

(def ^:private adult-boxes
  (into {} (map (fn [[t m]] [t [(:half m) (:height m)]])) types))

(defn- halved [[t [h hh]]]
  [t [(* 0.5 (double h)) (* 0.5 (double hh))]])

(def ^:private baby-boxes
  (into {} (map halved) adult-boxes))

(defn max-health [type] (:max-health (types type)))

(defn mob-type? [type] (contains? types type))

(defn breeding-item [type] (:breeding-item (types type)))

(defn- moody? [e]
  (pos? (long (or (:sound-variant e) 0))))

(defn sound-of
  "Returns the sound mob e makes for k, such as :say or :hurt.
  A moody cow has a voice of its own."
  [e k]
  (when-let [s (:sounds (types (:type e)))]
    (keyword (name (if (and (= :cow s) (moody? e)) :cow-moody s))
             (name k))))

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

(def ^:private cow-meta
  (into {} (for [v cow-variants s [:classic :moody]
                 baby [false true] burning [false true]]
             [[v s baby burning]
              {:cow-variant v :cow-sound s
               :baby? baby :burning? burning}])))

(def ^:private mooshroom-meta
  (into {} (for [v [0 1] baby [false true] burning [false true]]
             [[v baby burning]
              {:variant v :baby? baby :burning? burning}])))

(defn burning? [e] (boolean (:burning? e)))

(defn metadata
  "Returns what clients see of mob e besides its movement."
  [e]
  (case (:type e)
    :sheep (sheep-meta e)
    :cow (cow-meta [(cow-variants (long (or (:color e) 0)))
                    (if (moody? e) :moody :classic)
                    (some? (:baby-until e)) (burning? e)])
    :mooshroom (mooshroom-meta
                [(long (or (:color e) 0))
                 (some? (:baby-until e)) (burning? e)])))

(defn new-mob
  "Returns a fresh mob of kind type at pos, with nothing on its mind."
  [type pos color tick]
  {:type        type
   :pos         pos
   :vel         [0.0 0.0 0.0]
   :yaw         0.0 :pitch 0.0 :on-ground false
   :color       color
   :task        nil
   :no-action   0
   :health      (max-health type)
   :health-sent (max-health type)
   :arrived     [(inc (* 2 (long tick))) nil]})

(def ^:private ^:const follow-spread 0.11485000000000001)

(defn- follow-bonus
  "Returns the share of follow range Mob.finalizeSpawn adds, drawn as
  RandomSource.triangle around zero from the keys ks."
  ^double [ks]
  (* follow-spread (- (random/of-key (conj ks :follow))
                      (random/of-key (conj ks :follow-2)))))

(defn egg-mob
  "Returns a mob hatched from a spawn egg in level dim.
  The keys ks decide its colour, voice, yaw and follow range."
  [type pos ks tick dim]
  (let [color-fn (get-in types [type :spawn-color] (fn [_ _] 0))
        voices (long (get-in types [type :sound-variants] 1))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)
        voice (long (* voices (random/of-key (conj ks :voice))))
        color (color-fn ks (biome/at dim pos))]
    (assoc (new-mob type pos color tick)
      :yaw yaw :head-yaw yaw :sound-variant voice
      :follow-bonus (follow-bonus ks))))

(defn command-mob
  "Returns a mob summoned by a command in level dim. It faces as its
  constructor turns it, less than 2 pi degrees; the keys ks decide
  its colour and voice."
  [type pos ks tick dim]
  (let [r (double (float (random/of-key (conj ks :yaw))))
        yaw (double (float (* r (double (float (* 2.0 Math/PI))))))]
    (assoc (egg-mob type pos ks tick dim) :yaw yaw :head-yaw yaw)))

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
  "Returns true when dead mob e leaves at the start of this tick, as
  LivingEntity.tickDeath, before its step."
  [e]
  (and (not (pos? (double (:health e 1.0))))
       (>= (inc (long (or (:death-time e) 0))) death-ticks)))

(defn box-of
  "Returns the half width and the height of mob e.
  A baby measures half a grown mob."
  [e]
  ((if (baby? e) baby-boxes adult-boxes) (:type e)))

(defn eye-height
  "Returns how far above its position mob e looks out."
  ^double [e]
  (let [m (types (:type e))]
    (double (if (baby? e) (:baby-eye m) (:eye m)))))

(defn loot-entity
  "Returns mob e as the predicates of its loot table see it."
  [e]
  (let [n (long (or (:color e) 0))
        shroom (if (= 1 n) :brown :red)
        components (case (:type e)
                     :sheep {:sheep/color (dye-colors n)}
                     :mooshroom {:mooshroom/variant shroom}
                     nil)]
    (cond-> {:type (:type e) :baby? (baby? e)
             :sheared? (boolean (:sheared? e))}
      components (assoc :components components))))

(defn panicking? [e t] (< (long t) (long (or (:panic-until e) 0))))
