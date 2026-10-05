(ns collider.proto.entitydata
  "Synched fields of the entity classes and their wire form."
  (:require [collider.proto.buf :as buf]
            [collider.proto.codec :as c]
            [collider.proto.components :as comps]
            [collider.proto.nbt :as nbt]
            [collider.proto.text :as text])
  (:import (collider.proto Buf)))

(set! *warn-on-reflection* true)

(def classes
  "The parent and the synched fields of each entity class, in order."
  {:entity
   {:fields [[:shared-flags :byte 0]
             [:air-supply :int 300]
             [:custom-name :optional-component nil]
             [:custom-name-visible :boolean false]
             [:silent :boolean false]
             [:no-gravity :boolean false]
             [:pose :pose 0]
             [:ticks-frozen :int 0]]}
   :living
   {:parent :entity
    :fields [[:living-flags :byte 0]
             [:health :float 1.0]
             [:effect-particles :particles []]
             [:effect-ambience :boolean false]
             [:arrow-count :int 0]
             [:stinger-count :int 0]
             [:sleeping-pos :optional-block-pos nil]]}
   :mob {:parent :living :fields [[:mob-flags :byte 0]]}
   :pathfinder-mob {:parent :mob :fields []}
   :ageable-mob
   {:parent :pathfinder-mob
    :fields [[:baby :boolean false] [:age-locked :boolean false]]}
   :animal {:parent :ageable-mob :fields []}
   :sheep {:parent :animal :fields [[:wool :byte 0]]}
   :abstract-cow {:parent :animal :fields []}
   :cow
   {:parent :abstract-cow
    :fields [[:variant :cow-variant :temperate]
             [:sound-variant :cow-sound-variant :classic]]}
   :mushroom-cow {:parent :abstract-cow :fields [[:type :int 0]]}
   :pig
   {:parent :animal
    :fields [[:boost-time :int 0]
             [:variant :pig-variant :temperate]
             [:sound-variant :pig-sound-variant :classic]]}
   :rabbit {:parent :animal :fields [[:type :int 0]]}
   :armadillo
   {:parent :animal :fields [[:state :armadillo-state 0]]}
   :goat
   {:parent :animal
    :fields [[:screaming :boolean false] [:left-horn :boolean true]
             [:right-horn :boolean true]]}
   :chicken
   {:parent :animal
    :fields [[:variant :chicken-variant :temperate]
             [:sound-variant :chicken-sound-variant :classic]]}
   :avatar
   {:parent :living
    :fields [[:main-hand :humanoid-arm :right]
             [:mode-customisation :byte 0]]}
   :player
   {:parent :avatar
    :fields [[:absorption :float 0.0]
             [:score :int 0]
             [:shoulder-parrot-left :optional-unsigned-int nil]
             [:shoulder-parrot-right :optional-unsigned-int nil]]}
   :item-entity {:parent :entity :fields [[:item :item nil]]}
   :experience-orb {:parent :entity :fields [[:value :int 0]]}
   :primed-tnt
   {:parent :entity
    :fields [[:fuse :int 80] [:block-state :block-state :tnt]]}
   :falling-block
   {:parent :entity :fields [[:start-pos :block-pos [0 0 0]]]}
   :projectile {:parent :entity :fields []}
   :throwable-projectile {:parent :projectile :fields []}
   :throwable-item-projectile
   {:parent :throwable-projectile :fields [[:item :item nil]]}
   :hanging-entity
   {:parent :entity :fields [[:direction :direction 3]]}
   :item-frame
   {:parent :hanging-entity
    :fields [[:item :item nil] [:rotation :int 0]]}
   :painting
   {:parent :hanging-entity
    :fields [[:variant :painting-variant nil]]}
   :area-effect-cloud
   {:parent :entity
    :fields [[:radius :float 3.0]
             [:waiting :boolean false]
             [:particle :particle :entity-effect]]}})

(defn- chain [cls]
  (if-let [p (:parent (classes cls))] (conj (chain p) cls) [cls]))

(defn- numbered [cls]
  (into {}
        (map-indexed (fn [i [n t d]] [n [i t d]]))
        (mapcat #(:fields (classes %)) (chain cls))))

(def fields
  "The index, type and default of each field, by class."
  (into {} (map (fn [c] [c (numbered c)])) (keys classes)))

(defn default
  "Returns what the class holds in the field before anything sets it."
  [cls k]
  (nth (get-in fields [cls k]) 2))

(defn- entry [cls fs [k v]]
  (if-let [[i t] (fs k)]
    [i t v]
    (throw (ex-info "no such synched field" {:class cls :field k}))))

(defn entries
  "Returns the entity data of class cls for field values m.
  The entries come in index order."
  [cls m]
  (let [fs (fields cls)]
    (vec (sort-by first (map #(entry cls fs %) m)))))

(def data-types
  "The place of each entity data type in serializer order."
  {:byte 0 :int 1 :float 3 :optional-component 6 :item 7
   :boolean 8 :block-pos 10 :optional-block-pos 11 :direction 12
   :block-state 14 :particle 16 :pose 20 :cow-variant 23
   :cow-sound-variant 24 :pig-variant 28 :pig-sound-variant 29
   :chicken-variant 30 :chicken-sound-variant 31
   :painting-variant 34 :armadillo-state 36})

(defn- write-data-pos [^Buf buf v]
  (let [[x y z] v]
    (c/write-block-pos buf (long x) (long y) (long z))))

(defn- write-optional! [^Buf buf v write]
  (buf/write-boolean! buf (some? v))
  (when (some? v) (write buf v)))

(defn- write-data-value [^Buf buf type v]
  (case type
    :byte (buf/write-byte! buf (int v))
    :float (buf/write-float! buf (float v))
    :optional-component (write-optional! buf v text/write-component)
    :item (comps/write-item-stack buf v)
    :boolean (buf/write-boolean! buf (boolean v))
    :block-pos (write-data-pos buf v)
    :optional-block-pos (write-optional! buf v write-data-pos)
    :particle (comps/write-particle buf v)
    :painting-variant (comps/write-painting-variant buf v)
    (:int :direction :block-state :pose :cow-variant
     :cow-sound-variant :pig-variant :pig-sound-variant
     :chicken-variant :chicken-sound-variant :armadillo-state)
    (c/write-varint buf (long v))))

(def ^:private ^:const entity-data-end 255)

(defn write-entity-data
  [^Buf buf entries]
  (doseq [[idx type v] entries]
    (buf/write-byte! buf (int idx))
    (c/write-varint buf (data-types type))
    (write-data-value buf type v))
  (buf/write-byte! buf entity-data-end))

(def ^:private data-type-names
  (into {} (map (fn [[k v]] [(long v) k])) data-types))

(defn- read-optional-pos [^Buf buf]
  (when (buf/read-boolean buf) (c/read-block-pos buf)))

(defn- read-optional-text [^Buf buf]
  (when (buf/read-boolean buf) (nbt/read-nbt buf)))

(defn- read-data-value [^Buf buf type]
  (case type
    :byte (buf/read-byte buf)
    :float (buf/read-float buf)
    :optional-component (read-optional-text buf)
    :item (comps/read-item-stack buf)
    :boolean (buf/read-boolean buf)
    :block-pos (c/read-block-pos buf)
    :optional-block-pos (read-optional-pos buf)
    :particle (comps/read-particle buf)
    :painting-variant (comps/read-painting-variant buf)
    (:int :direction :block-state :pose :cow-variant
     :cow-sound-variant :pig-variant :pig-sound-variant
     :chicken-variant :chicken-sound-variant :armadillo-state)
    (c/read-varint buf)))

(defn read-entity-data
  [^Buf buf]
  (loop [out []]
    (let [idx (buf/read-unsigned-byte buf)]
      (if (= entity-data-end idx)
        out
        (let [type (data-type-names (c/read-varint buf))
              v (read-data-value buf type)]
          (recur (conj out [idx type v])))))))

