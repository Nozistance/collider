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

(defn- spawn-config [biome]
  (let [n (:name biome)
        k (cond (@warm-biomes n) :warm
                (@cold-biomes n) :cold
                :else :temperate)]
    (spawn-configs k)))

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

(defn- sounds [type]
  (into {} (for [k [:say :step :hurt :death]] [k (keyword (name type) (name k))])))

(def ^:private cow
  (merge (sounds :cow)
         {:half          0.45 :height 1.4 :speed 0.2
          :max-health    10.0
          :breeding-item :wheat
          :speeds        {:panic 2.0 :tempt 1.25 :follow 1.25}}))

(def types
  {:sheep     (merge (sounds :sheep)
                     {:half          0.45 :height 1.3 :speed 0.23
                      :max-health    8.0
                      :breeding-item :wheat
                      :speeds        {:panic 1.25 :tempt 1.1 :follow 1.1}
                      :spawn-color   sheep-color})
   :cow       cow
   :mooshroom (assoc cow :ground :mycelium)})

(defn egg-type [item]
  (let [t (get-in (data/items) [item :spawns])]
    (when (contains? types t) t)))

(defn max-health [type] (get-in types [type :max-health]))

(defn mob-type? [type] (contains? types type))

(defn breeding-item [type] (get-in types [type :breeding-item]))

(defn say-sound [type] (get-in types [type :say]))

(defn step-sound [type] (get-in types [type :step]))

(defn hurt-sound [type] (get-in types [type :hurt]))

(defn death-sound [type] (get-in types [type :death]))

(def ^:private sheep-meta
  (into {} (for [color (range 16) baby [false true] burning [false true] sheared [false true]]
             [[color baby burning sheared]
              (cond-> {:color color :baby? baby :burning? burning} sheared (assoc :sheared? true))])))

(defn burning? [e] (boolean (:burning? e)))

(defn metadata [e]
  (case (:type e)
    :sheep (sheep-meta [(long (or (:color e) 0))
                        (some? (:baby-until e))
                        (burning? e)
                        (boolean (:sheared? e))])
    :cow {:baby? (some? (:baby-until e)) :burning? (burning? e)}
    :mooshroom {:baby? (some? (:baby-until e)) :burning? (burning? e) :variant (long (or (:color e) 0))}))

(defn new-mob [type pos color tick]
  {:type        type
   :pos         pos
   :vel         [0.0 0.0 0.0]
   :yaw         0.0 :pitch 0.0 :on-ground false
   :color       color
   :task        nil
   :health      (max-health type)
   :health-sent (max-health type)})

(defn egg-mob
  "Returns a mob hatched from a spawn egg. The keys ks decide its
  colour and yaw."
  [type pos ks tick]
  (let [color-fn (get-in types [type :spawn-color] (fn [_ _] 0))
        yaw (- (* 360.0 (random/of-key (conj ks :yaw))) 180.0)]
    (assoc (new-mob type pos (color-fn ks (biome/at nil pos)) tick)
      :yaw yaw :head-yaw yaw)))

(defn exp-delay ^long [mean ^long t ^long eid kind]
  (max 1 (long (* (double mean) (- (Math/log (max 1.0E-9 (random/of-longs t eid (hash kind)))))))))

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
  (let [color (dye-colors (long (or (:color e) 0)))]
    (cond-> {:type (:type e) :baby? (baby? e)
             :sheared? (boolean (:sheared? e))}
            (= :sheep (:type e))
            (assoc :components {:sheep/color color}))))

(defn panicking? [e t] (< (long t) (long (or (:panic-until e) 0))))
