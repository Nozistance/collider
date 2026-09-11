(ns collider.game.blockentity
  (:require [clojure.string :as str]
            [collider.data :as data]
            [collider.game.sign :as sign]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private block-kinds
  {:banner :banner :wall-banner :banner
   :skull :skull :wall-skull :skull
   :wither-skull :skull :wither-wall-skull :skull
   :piglinwallskull :skull
   :player-head :skull :player-wall-head :skull
   :decorated-pot :decorated-pot
   :jukebox :jukebox
   :shelf :shelf
   :chiseled-book-shelf :chiseled-bookshelf
   :bell :bell
   :chest :chest :copper-chest :chest :weathering-copper-chest :chest
   :trapped-chest :trapped-chest
   :ender-chest :ender-chest
   :barrel :barrel})

(def ^:private silent #{:chiseled-bookshelf :bell :jukebox :chest :trapped-chest :ender-chest :barrel})

(def container-kinds #{:chest :trapped-chest :barrel})

(defn kind [^long st]
  (or (sign/kind st) (get block-kinds (block/type-of st))))

(defn at [world pos]
  (get-in world [:block-entities (chunk/block-chunk pos) pos]))

(defn type-id ^long [e]
  (data/registry-id "block_entity_type" (:kind e)))

(defn identifier [item]
  (str "minecraft:" (str/replace (name item) "-" "_")))

(defn stack-nbt [stack]
  (when stack
    {:id (identifier (:item stack)) :count (int (:count stack 1))}))

(defn items-nbt [items]
  (into [] (keep-indexed (fn [i s] (when s (assoc (stack-nbt s) :Slot (byte i))))) items))

(defn- dye-name [color] (str/replace (name color) "-" "_"))

(defn- pattern-nbt [{:keys [pattern color]}]
  {:pattern (if (map? pattern)
              {:asset_id (identifier (:asset (:direct pattern)))
               :translation_key (:translation-key (:direct pattern))}
              (identifier pattern))
   :color (dye-name color)})

(defn- banner-nbt [e]
  (cond-> {} (seq (:patterns e)) (assoc :patterns (mapv pattern-nbt (:patterns e)))))

(defn- uuid-ints ^ints [^java.util.UUID u]
  (int-array [(unsigned-bit-shift-right (.getMostSignificantBits u) 32)
              (.getMostSignificantBits u)
              (unsigned-bit-shift-right (.getLeastSignificantBits u) 32)
              (.getLeastSignificantBits u)]))

(defn- property-nbt [{:keys [name value signature]}]
  (cond-> (array-map :name name :value value) signature (assoc :signature signature)))

(defn- skin-nbt [skin]
  (cond-> {}
    (:body skin)   (assoc :texture (identifier (:body skin)))
    (:cape skin)   (assoc :cape (identifier (:cape skin)))
    (:elytra skin) (assoc :elytra (identifier (:elytra skin)))
    (:model skin)  (assoc :model (name (:model skin)))))

(defn profile-nbt [profile]
  (let [p (or (:left (:profile profile)) (:right (:profile profile)))]
    (merge (cond-> {}
             (:name p) (assoc :name (:name p))
             (:id p) (assoc :id (uuid-ints (:id p)))
             (seq (:properties p)) (assoc :properties (mapv property-nbt (:properties p))))
           (skin-nbt (:skin profile)))))

(defn- skull-nbt [e]
  (cond-> {} (:profile e) (assoc :profile (profile-nbt (:profile e)))))

(defn- pot-nbt [e]
  (cond-> {}
    (seq (:sherds e)) (assoc :sherds (mapv identifier (:sherds e)))
    (:item e) (assoc :item (stack-nbt (:item e)))))

(defn- shelf-nbt [e]
  {:Items (items-nbt (:items e)) :align_items_to_bottom (boolean (:align-bottom? e))})

(defn nbt [e]
  (case (:kind e)
    (:sign :hanging-sign) (sign/nbt e)
    :banner (banner-nbt e)
    :skull (skull-nbt e)
    :decorated-pot (pot-nbt e)
    :shelf (shelf-nbt e)
    {}))

(defn on-wire? [e]
  (not (contains? silent (:kind e))))

(def ^:private component-fields
  {:banner        {:patterns :banner-patterns}
   :skull         {:profile :profile}
   :decorated-pot {:sherds :pot-decorations}})

(defn from-stack [e stack]
  (reduce-kv (fn [e field component]
               (if-let [v (get-in stack [:components component])]
                 (assoc e field v)
                 e))
             e (get component-fields (:kind e) {})))

(defn to-stack [item e]
  (let [cs (reduce-kv (fn [m field component]
                        (let [v (get e field)]
                          (if (or (nil? v) (and (coll? v) (empty? v)))
                            m
                            (assoc m component v))))
                      {} (get component-fields (:kind e) {}))]
    (cond-> {:item item :count 1} (seq cs) (assoc :components cs))))

(defn fresh [k editor]
  (case k
    (:sign :hanging-sign) (sign/fresh k editor)
    :banner {:kind :banner :patterns []}
    :skull {:kind :skull}
    :decorated-pot {:kind :decorated-pot :sherds [] :item nil}
    :jukebox {:kind :jukebox :record nil}
    :shelf {:kind :shelf :items [nil nil nil]}
    :chiseled-bookshelf {:kind :chiseled-bookshelf :items [nil nil nil nil nil nil] :last-slot -1}
    :bell {:kind :bell}
    (:chest :trapped-chest :barrel) {:kind k :items (vec (repeat 27 nil))}
    :ender-chest {:kind :ender-chest}))

(defn wire [entries]
  (into {} (map (fn [[pos e]] [pos {:type (type-id e) :nbt (nbt e)}])) entries))
