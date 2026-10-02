(ns collider.game.block.blockentity
  "Block entities, the data of a block besides its state."
  (:require [collider.data :as data]
            [collider.game.block.sign :as sign]
            [collider.game.stack :as stack]
            [collider.world.block :as block]
            [collider.world.chunk :as chunk])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(def ^:private block-kinds
  {:banner              :banner :wall-banner :banner
   :skull               :skull :wall-skull :skull
   :wither-skull        :skull :wither-wall-skull :skull
   :piglinwallskull     :skull
   :player-head         :skull :player-wall-head :skull
   :decorated-pot       :decorated-pot
   :jukebox             :jukebox
   :shelf               :shelf
   :chiseled-book-shelf :chiseled-bookshelf
   :bell                :bell
   :chest               :chest :copper-chest :chest
   :weathering-copper-chest :chest
   :trapped-chest       :trapped-chest
   :ender-chest         :ender-chest
   :barrel              :barrel
   :shulker-box         :shulker-box
   :lectern             :lectern
   :furnace             :furnace
   :blast-furnace       :blast-furnace
   :smoker              :smoker
   :brewing-stand       :brewing-stand
   :campfire            :campfire
   :potent-sulfur       :potent-sulfur
   :enchantment-table   :enchanting-table
   :end-portal          :end-portal
   :end-gateway         :end-gateway
   :beacon              :beacon
   :conduit             :conduit
   :spawner             :mob-spawner
   :trial-spawner       :trial-spawner
   :vault               :vault
   :brushable           :brushable-block
   :copper-golem-statue :copper-golem-statue
   :weathering-copper-golem-statue :copper-golem-statue
   :command             :command-block
   :structure           :structure-block
   :jigsaw              :jigsaw
   :test                :test-block
   :test-instance       :test-instance-block})

(def container-kinds #{:chest :trapped-chest :barrel :shulker-box})

(def furnace-kinds #{:furnace :blast-furnace :smoker})

(def menu-kinds
  (conj (into container-kinds furnace-kinds) :brewing-stand))

(defn kind [^long st]
  (or (sign/kind st) (get block-kinds (block/type-of st))))

(defn removed?
  "Returns true when the block entity of old goes as st takes its
  cell with flags. The block and the kind of block entity change,
  and flag 256 is not set."
  [old st flags]
  (let [k (kind old)]
    (boolean
      (and k (not (bit-test (long flags) 8))
           (not= (block/block-of old) (block/block-of (long st)))
           (not= k (kind (long st)))))))

(defn at [world pos]
  (get-in world [:block-entities (chunk/block-chunk pos) pos]))

(defn type-id ^long [e]
  (data/registry-id "block_entity_type" (:kind e)))

(defn identifier [item]
  (data/wire item))

(defn stack-nbt [stack]
  (when stack
    {:id (identifier (:item stack)) :count (int (:count stack 1))}))

(defn items-nbt [items]
  (let [slot (fn [i s] (when s (assoc (stack-nbt s) :Slot (byte i))))]
    (into [] (keep-indexed slot) items)))

(defn- dye-name [color] (data/snake color))

(defn- pattern-nbt [{:keys [pattern color]}]
  {:pattern (if (map? pattern)
              {:asset_id (identifier (:asset (:direct pattern)))
               :translation_key (:translation-key (:direct pattern))}
              (identifier pattern))
   :color   (dye-name color)})

(defn- banner-nbt [e]
  (cond-> {}
    (seq (:patterns e))
    (assoc :patterns (mapv pattern-nbt (:patterns e)))))

(defn- uuid-ints ^ints [^UUID u]
  (let [hi (.getMostSignificantBits u)
        lo (.getLeastSignificantBits u)]
    (int-array [(unsigned-bit-shift-right hi 32) hi
                (unsigned-bit-shift-right lo 32) lo])))

(defn- property-nbt [{:keys [name value signature]}]
  (cond-> (array-map :name name :value value)
    signature (assoc :signature signature)))

(defn- skin-nbt [skin]
  (cond-> {}
          (:body skin) (assoc :texture (identifier (:body skin)))
          (:cape skin) (assoc :cape (identifier (:cape skin)))
          (:elytra skin) (assoc :elytra (identifier (:elytra skin)))
          (:model skin) (assoc :model (name (:model skin)))))

(defn- properties-nbt [p]
  (cond-> {}
    (:name p) (assoc :name (:name p))
    (:id p) (assoc :id (uuid-ints (:id p)))
    (seq (:properties p))
    (assoc :properties (mapv property-nbt (:properties p)))))

(defn profile-nbt [profile]
  (let [p (or (:left (:profile profile)) (:right (:profile profile)))]
    (merge (properties-nbt p) (skin-nbt (:skin profile)))))

(defn- skull-nbt [e]
  (cond-> {}
    (:profile e) (assoc :profile (profile-nbt (:profile e)))))

(defn- pot-nbt [e]
  (cond-> {}
          (seq (:sherds e))
          (assoc :sherds (mapv identifier (:sherds e)))
          (:item e) (assoc :item (stack-nbt (:item e)))))

(defn- shelf-nbt [e]
  {:Items (items-nbt (:items e))
   :align_items_to_bottom (boolean (:align-bottom? e))})

(defn- fresh-tags [k] (get (data/block-entities) k))

(defn- tags [e which] (get (fresh-tags (:kind e)) which))

(defn- age ^long [e t] (- (long t) (long (:born e))))

(defn nbt [e t]
  (case (:kind e)
    (:sign :hanging-sign) (sign/nbt e)
    :banner (banner-nbt e)
    :skull (skull-nbt e)
    :decorated-pot (pot-nbt e)
    :shelf (shelf-nbt e)
    :campfire {:Items (items-nbt (:items e))}
    :end-gateway (assoc (tags e :update) :Age (age e t))
    :structure-block (assoc (tags e :update) :author (:author e ""))
    (tags e :update)))

(def ^:private fixed
  #{:beacon :conduit :mob-spawner :trial-spawner :vault
    :brushable-block :copper-golem-statue :enchanting-table
    :end-portal})

(defn- custom [e t]
  (cond
    (= :end-gateway (:kind e)) (assoc (tags e :custom) :Age (age e t))
    (fixed (:kind e)) (tags e :custom)))

(defn entity-data [e t]
  (when-let [d (not-empty (custom e t))]
    {:type (:kind e) :data d}))

(defn on-wire? [e]
  (boolean (tags e :update-packet?)))

(def ^:private component-fields
  {:banner        {:patterns :banner-patterns}
   :skull         {:profile :profile}
   :decorated-pot {:sherds :pot-decorations}})

(defn contents [items]
  (let [last-at (fn [acc [i s]] (if s (long i) acc))
        top (reduce last-at -1 (map-indexed vector items))]
    (mapv stack/template (take (inc top) items))))

(defn- items-of [cs size]
  (vec (take size (concat (map stack/of-template cs) (repeat nil)))))

(defn from-stack [e stack]
  (reduce-kv (fn [e field component]
               (if-let [v (get-in stack [:components component])]
                 (assoc e field v)
                 e))
             (if (= :shulker-box (:kind e))
               (let [cs (get-in stack [:components :container])]
                 (assoc e :items (items-of cs 27)))
               e)
             (get component-fields (:kind e) {})))

(defn placed-by
  "Returns e as its block placed by player p leaves it: a structure
  block keeps the name of its author."
  [e p]
  (cond-> e
    (= :structure-block (:kind e)) (assoc :author (:name p))))

(defn- blank? [v] (or (nil? v) (and (coll? v) (empty? v))))

(defn to-stack [item e]
  (let [put (fn [m field component]
              (let [v (get e field)]
                (if (blank? v) m (assoc m component v))))
        cs (reduce-kv put {} (get component-fields (:kind e) {}))]
    (cond-> {:item item :count 1}
            (= :shulker-box (:kind e))
            (assoc-in [:components :container] (contents (:items e)))
            (seq cs) (update :components merge cs))))

(def ^:private blank-furnace {:items [nil nil nil] :used {}})

(def ^:private blank
  {:banner             {:patterns []}
   :decorated-pot      {:sherds [] :item nil}
   :jukebox            {:record nil}
   :shelf              {:items [nil nil nil]}
   :chiseled-bookshelf {:items (vec (repeat 6 nil))}
   :lectern            {:book nil :page 0}
   :brewing-stand      {:items (vec (repeat 5 nil))}
   :campfire           {:items (vec (repeat 4 nil))}
   :furnace            blank-furnace
   :blast-furnace      blank-furnace
   :smoker             blank-furnace})

(def ^:private furnace-fields
  {:lit-remaining :lit_time_remaining :lit-total :lit_total_time
   :cook :cooking_time_spent :cook-total :cooking_total_time})

(def ^:private tag-fields
  {:chiseled-bookshelf {:last-slot :last_interacted_slot}
   :potent-sulfur      {:countdown :countdown}
   :brewing-stand      {:brew :BrewTime :fuel :Fuel}
   :campfire           {:cook :CookingTimes
                        :cook-total :CookingTotalTimes}
   :furnace            furnace-fields
   :blast-furnace      furnace-fields
   :smoker             furnace-fields})

(defn- tag-value [v] (if (number? v) (long v) (mapv long v)))

(defn- from-tags [k]
  (let [kept (:custom (fresh-tags k))]
    (update-vals (get tag-fields k {}) #(tag-value (get kept %)))))

(defn fresh [k editor]
  (case k
    (:sign :hanging-sign) (sign/fresh k editor)
    (:chest :trapped-chest :barrel :shulker-box)
    {:kind k :items (vec (repeat 27 nil))}
    (merge (blank k) (from-tags k) {:kind k})))

(defn made [k t]
  (cond-> (fresh k nil) (= :end-gateway k) (assoc :born t)))

(defn saved [e t]
  (if (= :end-gateway (:kind e))
    (-> e (dissoc :born) (assoc :age (age e t)))
    e))

(defn loaded [e t]
  (if (= :end-gateway (:kind e))
    (-> e (dissoc :age) (assoc :born (- (long t) (long (:age e)))))
    e))

(defn wire [entries t]
  (let [entry (fn [[pos e]] [pos {:type (type-id e) :nbt (nbt e t)}])]
    (into {} (map entry) entries)))
