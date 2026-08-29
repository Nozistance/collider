(ns collider.game.systems.chunks
  (:require [collider.game.state :as state]
            [collider.proto.packets.play :as play]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def view-radius 7)
(def send-per-tick 20)

(defn chunk-coord ^long [^double c]
  (bit-shift-right (long (Math/floor c)) 4))

(defn wanted-chunks [world cp]
  (let [r (long (get-in world [:config :view-distance] view-radius))
        [cx cz] (chunk/id->pos cp)]
    (into #{} (chunk/around-ids (long cx) (long cz) r))))

(defn- load-packet [world cp]
  (let [[cx cz] (chunk/id->pos cp)
        [bm data] (if-let [c (get-in world [:chunks cp])]
                    (chunk/encode-column c)
                    [gen/primary-bitmask gen/flat-column])]
    {:packet/key ::play/chunk-data :chunk-x cx :chunk-z cz
     :ground-up? true :bitmask bm :data data}))

(defn- unload-packet [cp]
  (let [[cx cz] (chunk/id->pos cp)]
    {:packet/key ::play/chunk-data :chunk-x cx :chunk-z cz
     :ground-up? true :bitmask 0 :data gen/unload-column}))

(defn- own-column? [world eid sent-chunks cp]
  (or (contains? (or sent-chunks #{}) cp)
      (nil? (:writable world))
      (contains? (:writable world) eid)))

(defn- send-cap
  ^long [world eid]
  (if (if-let [w (:writable world)] (contains? w eid) true)
    (long (get-in world [:config :chunk-send-rate] send-per-tick))
    0))

(defn- nearest-first [ids cp]
  (let [[pcx pcz] (chunk/id->pos cp)]
    (sort-by (fn [id]
               (let [[cx cz] (chunk/id->pos id)
                     dx (- (long cx) (long pcx))
                     dz (- (long cz) (long pcz))]
                 [(+ (* dx dx) (* dz dz)) id]))
             ids)))

(defn- restream-deltas [world eid cp sent-chunks]
  (let [want    (wanted-chunks world cp)
        add-all (vec (remove #(contains? sent-chunks %) want))
        add     (into [] (take (send-cap world eid)) (nearest-first add-all cp))
        drop    (sort (remove #(contains? want %) sent-chunks))]
    (concat
     [[:chunks-sent eid cp add drop (when (> (count add-all) (count add)) true)]]
     (map #(vector :send eid (load-packet world %)) add)
     (map #(vector :send eid (unload-packet %)) drop))))

(defn- spawn-look-deltas [_ eid pos yaw pitch]
  (let [[sx sy sz] (or pos state/spawn-pos)]
    [[:send eid {:packet/key ::play/position-look
                 :x sx :y sy :z sz
                 :yaw (float (or yaw 0.0)) :pitch (float (or pitch 0.0))
                 :flags 0}]
     [:spawned eid]]))

(defn- stream-deltas [world [eid {:keys [pos yaw pitch chunk-pos sent-chunks needs-spawn? chunks-pending?]}]]
  (let [[x _ z] pos
        cp (chunk/pos->id (chunk-coord x) (chunk-coord z))]
    (concat
     (when (or (not= cp chunk-pos) chunks-pending?)
       (restream-deltas world eid cp sent-chunks))
     (when (and needs-spawn? (own-column? world eid sent-chunks cp))
       (spawn-look-deltas world eid pos yaw pitch)))))

(defn chunk-streaming [world _events]
  (mapv (fn [entry] #(stream-deltas world entry))
        (state/player-entries world)))
