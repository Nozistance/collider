(ns collider.proto.packets
  "Readers and writers of every packet, by connection state and name."
  (:require [collider.data :as data]
            [collider.proto.buf :as buf]
            [collider.proto.chunk :as chunk]
            [collider.proto.codec :as c])
  (:import (collider.java Buf)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- write-registry-entries! [^Buf buf names]
  (c/write-varint buf (count names))
  (doseq [n names]
    (c/write-id buf n)
    (buf/write-boolean! buf false)))

(defn- write-spawn-info [^Buf buf m]
  (c/write-holder-ref buf (long (:dimension-type m)))
  (c/write-id buf :overworld)
  (buf/write-long! buf 0)
  (buf/write-byte! buf 1)
  (buf/write-byte! buf -1)
  (buf/write-boolean! buf false)
  (buf/write-boolean! buf true)
  (buf/write-boolean! buf false)
  (c/write-varint buf 0)
  (c/write-varint buf 63))

(def packets
  {[:handshake :intention]
   {:read (fn [^Buf buf] {:protocol (c/read-varint buf)
                          :address  (c/read-string buf 255)
                          :port     (buf/read-unsigned-short buf)
                          :next     (c/read-varint buf)})}

   [:status :status-response]
   {:write (fn [^Buf buf m] (c/write-string buf (:json m)))}
   [:status :ping-request]
   {:read (fn [^Buf buf] {:payload (buf/read-long buf)})}
   [:status :pong-response]
   {:write (fn [^Buf buf m] (buf/write-long! buf (long (:payload m))))}

   [:login :hello]
   {:read (fn [^Buf buf] {:name (c/read-string buf 16) :uuid (c/read-uuid buf)})}
   [:login :login-compression]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:threshold m))))}
   [:login :login-finished]
   {:write (fn [^Buf buf m]
             (c/write-uuid buf (:uuid m))
             (c/write-string buf (:name m))
             (c/write-varint buf 0)
             (c/write-uuid buf (UUID. 0 0)))}
   [:login :login-disconnect]
   {:write (fn [^Buf buf m] (c/write-string buf (:json m)))}

   [:configuration :custom-payload]
   {:write (fn [^Buf buf m] (c/write-id buf (:channel m)) (c/write-string buf (:value m)))}
   [:configuration :update-enabled-features]
   {:write (fn [^Buf buf m] (c/write-list buf (:features m) c/write-id))}
   [:configuration :select-known-packs]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:packs m)))
             (doseq [pack (:packs m)]
               (doseq [s pack] (c/write-string buf s))))}
   [:configuration :registry-data]
   {:write (fn [^Buf buf m]
             (c/write-id buf (:registry m))
             (write-registry-entries! buf (:names m)))}
   [:configuration :update-tags]
   {:write (fn [^Buf buf m]
             (let [tags (:tags m)]
               (c/write-varint buf (count tags))
               (doseq [[registry ts] tags]
                 (c/write-id buf registry)
                 (c/write-varint buf (count ts))
                 (doseq [[tag entries] ts]
                   (c/write-id buf tag)
                   (let [ids (keep #(try (data/entry-id registry %) (catch Exception _ nil)) entries)]
                     (c/write-varint buf (count ids))
                     (doseq [id ids] (c/write-varint buf id)))))))}
   [:configuration :finish-configuration]
   {:write (fn [_ _] nil)}
   [:configuration :disconnect]
   {:write (fn [^Buf buf m] (c/write-component buf (:text m)))}

   [:play :login]
   {:write (fn [^Buf buf m]
             (buf/write-int! buf (int (:eid m)))
             (buf/write-boolean! buf false)
             (c/write-list buf [:overworld] c/write-id)
             (c/write-varint buf (long (:max-players m)))
             (c/write-varint buf (long (:view-distance m)))
             (c/write-varint buf (long (:simulation-distance m)))
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf true)
             (buf/write-boolean! buf false)
             (write-spawn-info buf m)
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf false))}
   [:play :respawn]
   {:write (fn [^Buf buf m]
             (write-spawn-info buf m)
             (buf/write-byte! buf (int (:keep m 0))))}
   [:play :player-abilities]
   {:read  (fn [^Buf buf] {:flags (buf/read-byte buf)})
    :write (fn [^Buf buf m]
             (buf/write-byte! buf (int (:flags m)))
             (buf/write-float! buf (float (:flying-speed m)))
             (buf/write-float! buf (float (:walking-speed m))))}
   [:play :set-default-spawn-position]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-id buf :overworld)
               (c/write-block-pos buf x y z)
               (buf/write-float! buf (float (:yaw m 0.0)))
               (buf/write-float! buf (float (:pitch m 0.0)))))}
   [:play :set-health]
   {:write (fn [^Buf buf m]
             (buf/write-float! buf (float (:health m)))
             (c/write-varint buf (long (:food m)))
             (buf/write-float! buf (float (:saturation m))))}
   [:play :set-experience]
   {:write (fn [^Buf buf m]
             (buf/write-float! buf (float (:progress m)))
             (c/write-varint buf (long (:level m)))
             (c/write-varint buf (long (:total m))))}
   [:play :game-event]
   {:write (fn [^Buf buf m]
             (buf/write-byte! buf (int (:event m)))
             (buf/write-float! buf (float (:value m 0.0))))}
   [:play :commands]
   {:write (fn [^Buf buf {:keys [nodes]}]
             (c/write-varint buf (count nodes))
             (doseq [{:keys [type name parser props executable? children]} nodes]
               (buf/write-byte! buf (int (bit-or (case type :root 0 :literal 1 :argument 2)
                                            (if executable? 4 0))))
               (c/write-varint buf (count children))
               (doseq [c children] (c/write-varint buf (long c)))
               (when (not= :root type)
                 (c/write-string buf name))
               (when (= :argument type)
                 (c/write-varint buf (data/registry-id "command_argument_type" parser))
                 (case (clojure.core/name parser)
                   "brigadier:integer" (do (buf/write-byte! buf 3) (buf/write-int! buf (int (:min props))) (buf/write-int! buf (int (:max props))))
                   "brigadier:double" (do (buf/write-byte! buf 3) (buf/write-double! buf (double (:min props))) (buf/write-double! buf (double (:max props))))
                   "brigadier:string" (c/write-varint buf (long (:kind props 0)))
                   "time" (buf/write-int! buf (int (:min props 0)))
                   "entity" (buf/write-byte! buf (int (bit-or (if (:single? props) 1 0) (if (:players? props) 2 0))))
                   "resource" (c/write-string buf (:registry props))
                   nil)))
             (c/write-varint buf (dec (count nodes))))}
   [:play :command-suggestions]
   {:write (fn [^Buf buf {:keys [id start length matches]}]
             (c/write-varint buf (long id))
             (c/write-varint buf (long start))
             (c/write-varint buf (long length))
             (c/write-varint buf (count matches))
             (doseq [m matches]
               (c/write-string buf m)
               (buf/write-boolean! buf false)))}
   [:play :command-suggestion]
   {:read (fn [^Buf buf] {:id (c/read-varint buf) :text (c/read-string buf 32500)})}
   [:play :award-stats]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:stats m)))
             (doseq [[k n] (:stats m)
                     :let [type (keyword (namespace k)) key (keyword (name k))]]
               (c/write-varint buf (data/registry-id "stat_type" type))
               (c/write-varint buf (data/registry-id (case type
                                                       :custom "custom_stat"
                                                       :mined "block"
                                                       (:killed :killed-by) "entity_type"
                                                       "item")
                                                     key))
               (c/write-varint buf (long n))))}
   [:play :game-rule-values]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:values m)))
             (doseq [[k v] (:values m)]
               (c/write-string buf k)
               (c/write-string buf v)))}
   [:play :set-game-rule]
   {:read (fn [^Buf buf]
            {:entries (vec (repeatedly (c/read-count buf)
                                       #(vector (c/read-string buf) (c/read-string buf))))})}
   [:play :ping-request]
   {:read (fn [^Buf buf] {:payload (buf/read-long buf)})}
   [:play :pong-response]
   {:write (fn [^Buf buf m] (buf/write-long! buf (long (:payload m))))}
   [:play :change-difficulty]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:difficulty m 0)))
             (buf/write-boolean! buf (boolean (:locked m))))}
   [:play :server-data]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:motd m))
             (buf/write-boolean! buf false))}
   [:play :initialize-border]
   {:write (fn [^Buf buf m]
             (buf/write-double! buf (double (:center-x m 0.0)))
             (buf/write-double! buf (double (:center-z m 0.0)))
             (buf/write-double! buf (double (:size m)))
             (buf/write-double! buf (double (:size m)))
             (c/write-varlong buf 0)
             (c/write-varint buf (long (:max-size m)))
             (c/write-varint buf (long (:warning-blocks m 5)))
             (c/write-varint buf (long (:warning-time m 15))))}
   [:play :ticking-state]
   {:write (fn [^Buf buf m]
             (buf/write-float! buf (float (:rate m 20.0)))
             (buf/write-boolean! buf (boolean (:frozen? m))))}
   [:play :ticking-step]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:steps m 0))))}
   [:play :update-attributes]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-varint buf (count (:attributes m)))
             (doseq [[attr base mods] (:attributes m)]
               (c/write-varint buf (data/registry-id "attribute" attr))
               (buf/write-double! buf (double base))
               (c/write-varint buf (count mods))
               (doseq [[id amount op] mods]
                 (c/write-string buf (data/wire id))
                 (buf/write-double! buf (double amount))
                 (c/write-varint buf (long op)))))}
   [:play :set-time]
   {:write (fn [^Buf buf m]
             (buf/write-long! buf (long (:age m)))
             (c/write-varint buf 1)
             (c/write-varint buf (data/datapack-id "world_clock" :overworld))
             (c/write-varlong buf (long (:time m)))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 1.0)))}
   [:play :player-position]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m) [vx vy vz] (:vel m [0.0 0.0 0.0])]
               (c/write-varint buf (long (:teleport-id m)))
               (buf/write-double! buf (double x)) (buf/write-double! buf (double y)) (buf/write-double! buf (double z))
               (buf/write-double! buf (double vx)) (buf/write-double! buf (double vy)) (buf/write-double! buf (double vz))
               (buf/write-float! buf (float (:yaw m 0.0)))
               (buf/write-float! buf (float (:pitch m 0.0)))
               (buf/write-int! buf (int (:relative m 0)))))}
   [:play :accept-teleportation]
   {:read (fn [^Buf buf] {:id (c/read-varint buf)})}
   [:play :set-chunk-cache-center]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:cx m))) (c/write-varint buf (long (:cz m))))}
   [:play :level-chunk-with-light]
   {:write (fn [^Buf buf m] (chunk/write-chunk! buf (:cx m) (:cz m) (:chunk m) (:block-entities m)))}
   [:play :open-sign-editor]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (buf/write-boolean! buf (boolean (:front? m))))}
   [:play :block-event]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (buf/write-byte! buf (int (:action m)))
             (buf/write-byte! buf (int (:param m)))
             (c/write-varint buf (long (:block m))))}
   [:play :block-entity-data]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)] (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:type m)))
             (c/write-nbt buf (:nbt m)))}
   [:play :forget-level-chunk]
   {:write (fn [^Buf buf m]
             (buf/write-long! buf (bit-or (bit-and (long (:cx m)) 0xFFFFFFFF)
                                     (bit-shift-left (long (:cz m)) 32))))}
   [:play :chunk-batch-start]
   {:write (fn [_ _] nil)}
   [:play :chunk-batch-finished]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:size m))))}
   [:play :chunk-batch-received]
   {:read (fn [^Buf buf] {:rate (buf/read-float buf)})}
   [:play :keep-alive]
   {:read  (fn [^Buf buf] {:id (buf/read-long buf)})
    :write (fn [^Buf buf m] (buf/write-long! buf (long (:id m))))}
   [:play :disconnect]
   {:write (fn [^Buf buf m] (c/write-component buf (:text m)))}
   [:play :system-chat]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:text m))
             (buf/write-boolean! buf (boolean (:overlay m))))}
   [:play :block-update]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-block-pos buf (long x) (long y) (long z)))
             (c/write-varint buf (long (:state m))))}
   [:play :section-blocks-update]
   {:write (fn [^Buf buf m]
             (let [[sx sy sz] (:section m)]
               (buf/write-long! buf (c/section-pos (long sx) (long sy) (long sz))))
             (c/write-varint buf (count (:changes m)))
             (doseq [[at state] (:changes m)]
               (c/write-varlong buf (bit-or (bit-shift-left (long state) 12) (long at)))))}
   [:play :block-changed-ack]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:sequence m))))}
   [:play :player-info-update]
   {:write (fn [^Buf buf m]
             (if (= :latency (:action m))
               (do (buf/write-byte! buf 0x10)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-varint buf (long (or ping 0)))))
               (do (buf/write-byte! buf 0x1D)
                   (c/write-varint buf (count (:players m)))
                   (doseq [{:keys [uuid name gamemode ping]} (:players m)]
                     (c/write-uuid buf uuid)
                     (c/write-string buf name)
                     (c/write-varint buf 0)
                     (c/write-varint buf (long (or gamemode 1)))
                     (buf/write-boolean! buf true)
                     (c/write-varint buf (long (or ping 0)))))))}
   [:play :level-event]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (buf/write-int! buf (int (:event m)))
               (c/write-block-pos buf (long x) (long y) (long z))
               (buf/write-int! buf (int (:data m)))
               (buf/write-boolean! buf false)))}
   [:play :player-info-remove]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:uuids m)))
             (doseq [u (:uuids m)] (c/write-uuid buf u)))}
   [:play :tab-list]
   {:write (fn [^Buf buf m]
             (c/write-component buf (:header m))
             (c/write-component buf (:footer m)))}
   [:play :set-held-slot]
   {:write (fn [^Buf buf m] (c/write-varint buf (long (:slot m))))}
   [:play :container-set-content]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (c/write-varint buf (count (:items m)))
             (doseq [st (:items m)] (c/write-item-stack buf st))
             (c/write-item-stack buf (:carried m)))}
   [:play :container-set-slot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m 0)))
             (c/write-varint buf (long (:state-id m 0)))
             (buf/write-short! buf (int (:slot m)))
             (c/write-item-stack buf (:stack m)))}

   [:play :open-screen]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (c/write-varint buf (long (:menu m)))
             (c/write-component buf (:title m)))}
   [:play :container-set-data]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:container m)))
             (buf/write-short! buf (int (:id m)))
             (buf/write-short! buf (int (:value m))))}
   [:play :container-button-click]
   {:read (fn [^Buf buf] {:container (c/read-varint buf) :button (c/read-varint buf)})}
   [:play :update-recipes]
   {:write (fn [^Buf buf m]
             (let [sets (:property-sets m)
                   item (fn [i] (c/write-holder-ref buf (data/registry-id "item" i)))]
               (c/write-varint buf (count sets))
               (doseq [[k items] sets]
                 (c/write-id buf (data/kebab k))
                 (c/write-varint buf (count items))
                 (run! item items))
               (c/write-varint buf (count (:stonecutting m)))
               (doseq [{:keys [in out]} (:stonecutting m)]
                 (c/write-varint buf (inc (count in)))
                 (run! item in)
                 (c/write-varint buf (data/registry-id "slot_display" :item-stack))
                 (item (:item out))
                 (c/write-varint buf (long (:count out 1)))
                 (c/write-patch buf nil))))}
   [:play :container-close]
   {:read  (fn [^Buf buf] {:container (c/read-varint buf)})
    :write (fn [^Buf buf m] (c/write-varint buf (long (:container m))))}
   [:play :set-cursor-item]
   {:write (fn [^Buf buf m] (c/write-item-stack buf (:stack m)))}

   [:play :bundle-delimiter]
   {:write (fn [_ _] nil)}
   [:play :add-entity]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-uuid buf (:uuid m))
             (c/write-varint buf (long (:type m)))
             (c/write-vec3 buf (:pos m))
             (c/write-lp-vec3 buf (:vel m [0.0 0.0 0.0]))
             (c/write-angle buf (:pitch m 0.0))
             (c/write-angle buf (:yaw m 0.0))
             (c/write-angle buf (:head-yaw m 0.0))
             (c/write-varint buf (long (:data m 0))))}
   [:play :remove-entities]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (count (:eids m)))
             (doseq [e (:eids m)] (c/write-varint buf (long e))))}
   [:play :set-entity-data]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-entity-data buf (:data m)))}
   [:play :move-entity-pos]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m))) (buf/write-short! buf (int (:dy m))) (buf/write-short! buf (int (:dz m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-pos-rot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-short! buf (int (:dx m))) (buf/write-short! buf (int (:dy m))) (buf/write-short! buf (int (:dz m)))
             (buf/write-byte! buf (int (:yaw m))) (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :move-entity-rot]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:yaw m))) (buf/write-byte! buf (int (:pitch m)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :rotate-head]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:yaw m))))}
   [:play :entity-position-sync]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-vec3 buf (:pos m))
             (c/write-vec3 buf (:vel m [0.0 0.0 0.0]))
             (buf/write-float! buf (float (:yaw m 0.0)))
             (buf/write-float! buf (float (:pitch m 0.0)))
             (buf/write-boolean! buf (boolean (:on-ground m))))}
   [:play :set-entity-motion]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (c/write-lp-vec3 buf (:vel m)))}
   [:play :set-equipment]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (let [slots (vec (:slots m))]
               (doseq [[i [slot stack]] (map-indexed vector slots)]
                 (buf/write-byte! buf (int (if (< (inc i) (count slots)) (bit-or (long slot) 0x80) slot)))
                 (c/write-item-stack buf stack))))}
   [:play :animate]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-byte! buf (int (:action m))))}
   [:play :hurt-animation]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:eid m)))
             (buf/write-float! buf (float (:yaw m 0.0))))}
   [:play :entity-event]
   {:write (fn [^Buf buf m]
             (buf/write-int! buf (int (:eid m)))
             (buf/write-byte! buf (int (:event m))))}
   [:play :take-item-entity]
   {:write (fn [^Buf buf m]
             (c/write-varint buf (long (:item m)))
             (c/write-varint buf (long (:collector m)))
             (c/write-varint buf (long (:amount m 1))))}
   [:play :sound]
   {:write (fn [^Buf buf m]
             (let [[x y z] (:pos m)]
               (c/write-holder-ref buf (long (:sound m)))
               (c/write-varint buf (long (:source m)))
               (buf/write-int! buf (int (* 8.0 (double x))))
               (buf/write-int! buf (int (* 8.0 (double y))))
               (buf/write-int! buf (int (* 8.0 (double z))))
               (buf/write-float! buf (float (:volume m)))
               (buf/write-float! buf (float (:pitch m)))
               (buf/write-long! buf (long (:seed m 0)))))}
   [:play :level-particles]
   {:write (fn [^Buf buf m]
             (buf/write-boolean! buf false)
             (buf/write-boolean! buf false)
             (c/write-vec3 buf (:pos m))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float 0.0))
             (buf/write-float! buf (float (:speed m)))
             (buf/write-int! buf (int (:count m)))
             (c/write-varint buf (long (:particle m)))
             (when-some [st (:state m)] (c/write-varint buf (long st))))}
   [:play :explode]
   {:write (fn [^Buf buf m]
             (c/write-vec3 buf (:center m))
             (buf/write-float! buf (float (:radius m)))
             (buf/write-int! buf (int (:blocks m 0)))
             (if-let [k (:knockback m)]
               (do (buf/write-boolean! buf true) (c/write-vec3 buf k))
               (buf/write-boolean! buf false))
             (c/write-varint buf (long (:particle m)))
             (c/write-holder-ref buf (long (:sound m)))
             (c/write-varint buf (count (:block-particles m)))
             (doseq [[id scaling speed weight] (:block-particles m)]
               (c/write-varint buf (long id))
               (buf/write-float! buf (float scaling))
               (buf/write-float! buf (float speed))
               (c/write-varint buf (long weight))))}
   [:play :chat]
   {:read (fn [^Buf buf] {:message (c/read-string buf 256)})}
   [:play :chat-command]
   {:read (fn [^Buf buf] {:command (c/read-string buf)})}
   [:play :move-player-pos]
   {:read (fn [^Buf buf] {:pos   [(buf/read-double buf) (buf/read-double buf) (buf/read-double buf)]
                          :flags (buf/read-byte buf)})}
   [:play :move-player-pos-rot]
   {:read (fn [^Buf buf] {:pos   [(buf/read-double buf) (buf/read-double buf) (buf/read-double buf)]
                          :yaw   (buf/read-float buf)
                          :pitch (buf/read-float buf)
                          :flags (buf/read-byte buf)})}
   [:play :move-player-rot]
   {:read (fn [^Buf buf] {:yaw   (buf/read-float buf)
                          :pitch (buf/read-float buf)
                          :flags (buf/read-byte buf)})}
   [:play :move-player-status-only]
   {:read (fn [^Buf buf] {:flags (buf/read-byte buf)})}
   [:play :player-action]
   {:read (fn [^Buf buf] {:action   (c/read-varint buf)
                          :pos      (c/read-block-pos buf)
                          :face     (buf/read-unsigned-byte buf)
                          :sequence (c/read-varint buf)})}
   [:play :use-item-on]
   {:read (fn [^Buf buf] {:hand     (c/read-varint buf)
                          :pos      (c/read-block-pos buf)
                          :face     (c/read-varint buf)
                          :cursor   [(buf/read-float buf) (buf/read-float buf) (buf/read-float buf)]
                          :inside   (buf/read-boolean buf)
                          :border   (buf/read-boolean buf)
                          :sequence (c/read-varint buf)})}
   [:play :use-item]
   {:read (fn [^Buf buf] {:hand     (c/read-varint buf)
                          :sequence (c/read-varint buf)
                          :yaw      (buf/read-float buf)
                          :pitch    (buf/read-float buf)})}
   [:play :sign-update]
   {:read (fn [^Buf buf] {:pos    (c/read-block-pos buf)
                          :front? (buf/read-boolean buf)
                          :lines  (vec (repeatedly 4 #(c/read-string buf 384)))})}
   [:play :swing]
   {:read (fn [^Buf buf] {:hand (c/read-varint buf)})}
   [:play :player-command]
   {:read (fn [^Buf buf] {:eid    (c/read-varint buf)
                          :action (c/read-varint buf)
                          :data   (c/read-varint buf)})}
   [:play :player-input]
   {:read (fn [^Buf buf] {:flags (buf/read-byte buf)})}
   [:play :pick-item-from-block]
   {:read (fn [^Buf buf] {:pos          (c/read-block-pos buf)
                          :include-data (buf/read-boolean buf)})}
   [:play :pick-item-from-entity]
   {:read (fn [^Buf buf] {:id           (c/read-varint buf)
                          :include-data (buf/read-boolean buf)})}
   [:play :set-carried-item]
   {:read (fn [^Buf buf] {:slot (buf/read-short buf)})}
   [:play :container-click]
   {:read (fn [^Buf buf]
            (let [container (c/read-varint buf)
                  state-id (c/read-varint buf)
                  slot (buf/read-short buf)
                  button (buf/read-byte buf)
                  mode (c/read-varint buf)
                  changed (into {} (repeatedly (c/read-varint buf)
                                               #(vector (long (buf/read-short buf)) (c/read-hashed-stack buf))))
                  carried (c/read-hashed-stack buf)]
              {:container container :state-id state-id :slot slot :button button
               :mode      mode :changed changed :carried carried}))}
   [:play :set-creative-mode-slot]
   {:read (fn [^Buf buf] {:slot  (buf/read-short buf)
                          :stack (c/read-item-stack buf)})}
   [:play :client-command]
   {:read (fn [^Buf buf] {:action (c/read-varint buf)})}
   [:play :interact]
   {:read (fn [^Buf buf]
            (let [target (c/read-varint buf)
                  action (c/read-varint buf)]
              (case action
                0 (let [hand (c/read-varint buf)]
                    {:target target :action action :hand hand :sneaking (buf/read-boolean buf)})
                2 (let [at [(buf/read-float buf) (buf/read-float buf) (buf/read-float buf)]
                        hand (c/read-varint buf)]
                    {:target target :action action :at at :hand hand :sneaking (buf/read-boolean buf)})
                {:target target :action action :sneaking (buf/read-boolean buf)})))}})

(def ^:private ^:table inbound
  (delay
    (into {}
          (map (fn [[state dirs]]
                 [state (into {}
                              (map (fn [[nm id]]
                                     [(long id) (assoc (get packets [state nm]) :packet nm)]))
                              (:serverbound dirs))]))
          (data/packets))))

(def ^:private ^:table outbound
  (delay
    (into {}
          (map (fn [[state dirs]]
                 [state (into {}
                              (keep (fn [[nm id]]
                                      (when-let [w (:write (get packets [state nm]))]
                                        [nm {:id (long id) :write w}])))
                              (:clientbound dirs))]))
          (data/packets))))

(defn decode [state ^Buf buf]
  (let [id (c/read-varint buf)]
    (when-let [e (get (get @inbound state) id)]
      (if-let [r (:read e)]
        (assoc (r buf) :packet (:packet e))
        {:packet (:packet e)}))))

(defn encode! [state ^Buf buf m]
  (let [e (or (get (get @outbound state) (:packet m))
              (throw (ex-info "no writer for packet" {:state state :packet (:packet m)})))]
    (c/write-varint buf (long (:id e)))
    ((:write e) buf m)))
