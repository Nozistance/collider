(ns collider.tables.generate
  "Generating the game data tables from the vanilla server."
  (:require [clojure.java.io :as io]
            [collider.tables.brewing :as brewing]
            [collider.tables.classes :as classes]
            [collider.tables.fetch :as fetch]
            [collider.tables.files :as files]
            [collider.tables.items :as items]
            [collider.tables.loot :as loot]
            [collider.tables.recipes :as recipes]
            [collider.tables.registry :as registry]
            [collider.tables.reports :as reports]
            [collider.tables.stamp :as stamp]
            [collider.tables.tags :as tags]
            [collider.tables.worldgen :as worldgen]
            [collider.tables.progress :refer [progress! timed]])
  (:import (java.io File)
           (java.util.zip ZipFile)))

(set! *warn-on-reflection* true)

(def ^:private lang-file "assets/minecraft/lang/en_us.json")

(defn- write-edn! [dir k data]
  (let [f (io/file dir (str (name k) ".edn"))]
    (io/make-parents f)
    (with-open [w (io/writer f)]
      (binding [*out* w *print-length* nil *print-level* nil
                *print-namespace-maps* true]
        (pr data)
        (print "\n")))))

(defn- tagged-tables [zf reports rs {:keys [dyes synced]}]
  (let [dp (registry/datapack-names zf synced)
        regs (distinct (concat (keys rs) (map first dp)))
        tags (tags/tags-of zf regs)
        item-names (set (keys (get rs "item")))
        potion-names (set (keys (get rs "potion")))
        effect-names (set (keys (get rs "mob_effect")))]
    {:packets    (registry/packets reports)
     :registries rs
     :datapack   dp
     :enchantments (items/enchantments zf tags)
     :recipes (recipes/recipes zf tags dyes item-names potion-names)
     :potions    (brewing/potion-table potion-names)
     :effects    (brewing/effect-table effect-names)
     :tags       tags}))

(defn- item-table [zf from-class reports tags]
  (let [{:keys [compost walls remainders banners
                non-breakers]} from-class
        lang (files/read-json zf lang-file)]
    (merge-with merge (items/vanilla-items reports tags)
                compost walls remainders banners non-breakers
                (items/station-items reports tags lang))))

(defn- class-tables [zf from-class reports tags]
  (let [{:keys [props shapes placers]} from-class]
    {:blocks     (registry/blocks reports props shapes)
     :drops      (loot/block-drops zf)
     :entity-drops (loot/entity-drops zf)
     :items      (item-table zf from-class reports tags)
     :dimension-types (worldgen/dimension-types zf)
     :biomes     (worldgen/biomes zf)
     :features   (worldgen/features zf placers)}))

(defn- tables [zf from-class reports rs]
  (let [tagged (tagged-tables zf reports rs from-class)]
    (merge (dissoc from-class :props :compost :walls :placers
                   :remainders :banners :dyes :synced :non-breakers)
           tagged
           (class-tables zf from-class reports (:tags tagged)))))

(defn- write-tables! [^File server ^File reports out from-class sha]
  (io/delete-file (io/file out "stamp.edn") true)
  (with-open [zf (ZipFile/new server)]
    (let [ts (tables zf from-class reports
                     (registry/registries reports))
          n (count ts)]
      (doseq [[i [k data]] (map-indexed vector ts)]
        (progress! {:event :progress :step :tables
                    :done (inc i) :total n})
        (write-edn! out k data))
      (write-edn! out :stamp (assoc (stamp/stamp) :server-sha1 sha))
      {:count n :dir (str out)})))

(defn generate!
  "Writes every table into out, from jar or from Mojang when nil."
  [{:keys [out cache jar]}]
  (let [bundle (fetch/fetch cache jar)
        sha (fetch/sha1 bundle)
        dir (reports/reports bundle (io/file cache sha))
        tmp (files/temp-dir "game")]
    (try (let [server (reports/game-jar bundle tmp)]
           (timed :tables
             #(write-tables! server dir out
                (classes/read-classes bundle server) sha)))
         (finally (files/delete-tree! tmp)))))
