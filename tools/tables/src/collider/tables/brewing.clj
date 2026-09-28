(ns collider.tables.brewing
  "The effects, the potions and the mixes of a brewing stand."
  (:require [clojure.string :as str]
            [collider.tables.reflect
             :refer [call call-static elements hidden-field key-of
                     registry]]
            [collider.tables.value :refer [kw unknown]]))

(set! *warn-on-reflection* true)

(defn- category [e]
  (kw (str/lower-case (call (call e "getCategory") "name"))))

(defn- effect-entry [e]
  (sorted-map :category (category e)
              :color (call e "getColor")
              :instant? (call e "isInstantaneous")))

(defn effects
  "Returns the colour, category and immediacy of every effect."
  []
  (let [reg (registry "MOB_EFFECT")]
    (into (sorted-map)
          (map (fn [e] [(key-of reg e) (effect-entry e)]))
          (elements reg))))

(defn- holder-name [h] (kw (str (call (call h "key") "identifier"))))

(defn- instance [i]
  (sorted-map :amplifier (call i "getAmplifier")
              :duration (call i "getDuration")
              :effect (holder-name (call i "getEffect"))))

(defn potions
  "Returns the effect instances every potion gives."
  []
  (let [reg (registry "POTION")]
    (into (sorted-map)
          (map (fn [p]
                 [(key-of reg p)
                  (mapv instance (call p "getEffects"))]))
          (elements reg))))

(defn- ingredient-item [ing]
  (let [items (-> (call ing "items") (call "iterator") iterator-seq)]
    (when-not (= 1 (count items))
      (throw (unknown "brewing ingredient of many items"
                      {:ingredient (str ing)})))
    (holder-name (first items))))

(defn- mix [m]
  (let [field #(hidden-field (class m) m %)]
    {:from (holder-name (field "from"))
     :ingredient (ingredient-item (field "ingredient"))
     :to (holder-name (field "to"))}))

(defn brewing
  "Returns the containers and the mixes of a brewing stand of a
  server with the features enabled."
  [features]
  (let [b (call-static "world.item.alchemy.PotionBrewing" "bootstrap"
                       features)
        field #(hidden-field (class b) b %)]
    {:containers (mapv ingredient-item (field "containers"))
     :container-mixes (mapv mix (field "containerMixes"))
     :potion-mixes (mapv mix (field "potionMixes"))}))
