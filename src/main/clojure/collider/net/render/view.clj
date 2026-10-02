(ns collider.net.render.view
  "Chunk packets of the view of a player."
  (:require [collider.game.block.blockentity :as be]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private chunk-level-keys [:min-y :max-y :sky? :dim :chunks])

(defn- chunk-packet [world id]
  (let [[x z] (chunk/id->pos id)
        bes (get-in world [:block-entities id])]
    {:packet         :level-chunk-with-light :cx x :cz z
     :chunk          (get-in world [:chunks id] chunk/empty-chunk)
     :block-entities (be/wire bes (:tick world))
     :level          (select-keys world chunk-level-keys)}))

(defn forget-chunk-packet
  "Returns the packet that unloads chunk id."
  [id]
  (let [[x z] (chunk/id->pos id)]
    {:packet :forget-level-chunk :cx x :cz z}))

(defn- added-chunk-packets [world add]
  (when (seq add)
    (concat
      [{:packet :chunk-batch-start}]
      (map #(chunk-packet world %) add)
      [{:packet :chunk-batch-finished :size (count add)}])))

(defn center-packet
  "Returns the packet that centres the view on chunk id."
  [id]
  (let [[cx cz] (chunk/id->pos id)]
    {:packet :set-chunk-cache-center :cx cx :cz cz}))

(defn chunk-packets
  "Returns the packets of a :chunks-sent delta: the centre, the
  chunks dropped and the chunks added."
  [world [_ _ add drop center]]
  (concat
    (when center [(center-packet center)])
    (map forget-chunk-packet drop)
    (added-chunk-packets world add)))
