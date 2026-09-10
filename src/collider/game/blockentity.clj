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
   :bell :bell})

(def ^:private silent #{:chiseled-bookshelf :bell :jukebox})

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

(defn- banner-nbt [e]
  (cond-> {} (seq (:patterns e)) (assoc :patterns (vec (:patterns e)))))

(defn- skull-nbt [e]
  (cond-> {} (:profile e) (assoc :profile (:profile e))))

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

(defn fresh [k editor]
  (case k
    (:sign :hanging-sign) (sign/fresh k editor)
    :banner {:kind :banner :patterns []}
    :skull {:kind :skull}
    :decorated-pot {:kind :decorated-pot :sherds [] :item nil}
    :jukebox {:kind :jukebox :record nil}
    :shelf {:kind :shelf :items [nil nil nil]}
    :chiseled-bookshelf {:kind :chiseled-bookshelf :items [nil nil nil nil nil nil] :last-slot -1}
    :bell {:kind :bell}))

(defn wire [entries]
  (into {}
        (comp (filter (fn [[_ e]] (on-wire? e)))
              (map (fn [[pos e]] [pos {:type (type-id e) :nbt (nbt e)}])))
        entries))
