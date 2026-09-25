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
  "The cow variants by the id this project gives them."
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
  "The value of a number vanilla declares as a float."
  ^double [^double v] (double (float v)))

(def ^:private cow
  {:half           (attr 0.45) :height (attr 1.4)
   :speed          (attr 0.2)
   :max-health     10.0
   :sounds         :cow
   :sound-variants 2
   :breeding-item  :wheat
   :spawn-color    cow-variant})

(def types
  {:sheep     {:half          (attr 0.45)
               :height        (attr 1.3)
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

(defn max-health [type] (get-in types [type :max-health]))

(defn mob-type? [type] (contains? types type))

(defn breeding-item [type] (get-in types [type :breeding-item]))

(defn- moody? [e]
  (pos? (long (or (:sound-variant e) 0))))

(defn sound-of
  "Returns the sound mob e makes for k, one of :say, :step, :hurt
  and :death. A moody cow has a voice of its own."
  [e k]
  (when-let [s (get-in types [(:type e) :sounds])]
    (keyword (name (if (and (= :cow s) (moody? e)) :cow-moody s))
             (name k))))

(def ^:private sheep-meta
  (into {} (for [color (range 16) baby [false true]
                 burning [false true] sheared [false true]]
             [[color baby burning sheared]
              (cond-> {:color color :baby? baby :burning? burning}
                sheared (assoc :sheared? true))])))

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
    :sheep (sheep-meta [(long (or (:color e) 0))
                        (some? (:baby-until e))
                        (burning? e)
                        (boolean (:sheared? e))])
    :cow (cow-meta [(cow-variants (long (or (:color e) 0)))
                    (if (moody? e) :moody :classic)
                    (some? (:baby-until e)) (burning? e)])
    :mooshroom (mooshroom-meta
                [(long (or (:color e) 0))
                 (some? (:baby-until e)) (burning? e)])))

(defn new-mob
  "Returns a fresh mob of kind type at pos, with nothing on its
  mind."
  [type pos color tick]
  {:type        type
   :pos         pos
   :vel         [0.0 0.0 0.0]
   :yaw         0.0 :pitch 0.0 :on-ground false
   :color       color
   :task        nil
   :no-action   0
   :health      (max-health type)
   :health-sent (max-health type)})

(defn egg-mob
  "Returns a mob hatched from a spawn egg in level dim. The keys ks
  decide its colour, voice and yaw."
  [type pos ks tick dim]
  (let [color-fn (get-in types [type :spawn-color] (fn [_ _] 0))
        voices (long (get-in types [type :sound-variants] 1))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)
        voice (long (* voices (random/of-key (conj ks :voice))))
        color (color-fn ks (biome/at dim pos))]
    (assoc (new-mob type pos color tick)
      :yaw yaw :head-yaw yaw :sound-variant voice)))

(defn exp-delay
  "Returns a wait of at least one tick, drawn from an exponential
  law with the given mean."
  ^long [mean ^long t ^long eid kind]
  (let [r (max 1.0E-9 (random/of-longs t eid (hash kind)))]
    (max 1 (long (* (double mean) (- (Math/log r)))))))

(defn in-love? [e t] (> (long (or (:love-until e) 0)) (long t)))

(defn baby? [e] (some? (:baby-until e)))

(defn box-of
  "Returns the half width and the height of mob e.
  A baby measures half a grown mob."
  [e]
  (let [{:keys [half height]} (types (:type e))]
    (if (baby? e)
      [(* 0.5 (double half)) (* 0.5 (double height))]
      [half height])))

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
