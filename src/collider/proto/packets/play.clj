(ns collider.proto.packets.play
  (:require [collider.proto.spec :as spec :refer [defpacket]]
            [collider.proto.types :as t])
  (:import (io.netty.buffer ByteBuf)))

(set! *warn-on-reflection* true)

(defpacket ::keep-alive {:state :play :dir :s->c :id 0x00}
  [:id :varint])

(defpacket ::join-game {:state :play :dir :s->c :id 0x01}
  [:entity-id      :int]
  [:gamemode       :ubyte]
  [:dimension      :byte]
  [:difficulty     :ubyte]
  [:max-players    :ubyte]
  [:level-type     :string]
  [:reduced-debug? :bool])

(defpacket ::time-update {:state :play :dir :s->c :id 0x03}
  [:world-age :long]
  [:time      :long])

(defpacket ::spawn-position {:state :play :dir :s->c :id 0x05}
  [:location :position])

(defpacket ::update-health {:state :play :dir :s->c :id 0x06}
  [:health     :float]
  [:food       :varint]
  [:saturation :float])

(defpacket ::respawn {:state :play :dir :s->c :id 0x07}
  [:dimension  :int]
  [:difficulty :ubyte]
  [:gamemode   :ubyte]
  [:level-type :string])

(defpacket ::position-look {:state :play :dir :s->c :id 0x08}
  [:x     :double]
  [:y     :double]
  [:z     :double]
  [:yaw   :float]
  [:pitch :float]
  [:flags :byte])

(defpacket ::chunk-data {:state :play :dir :s->c :id 0x21}
  [:chunk-x    :int]
  [:chunk-z    :int]
  [:ground-up? :bool]
  [:bitmask    :ushort]
  [:data       :bytes])

(defpacket ::disconnect {:state :play :dir :s->c :id 0x40}
  [:reason :string])

(defpacket ::tab-header {:state :play :dir :s->c :id 0x47}
  [:header :string]
  [:footer :string])

(defpacket ::chat {:state :play :dir :s->c :id 0x02}
  [:json     :string]
  [:position :byte])

(defpacket ::player-abilities {:state :play :dir :s->c :id 0x39}
  [:flags        :byte]
  [:flying-speed :float]
  [:fov-modifier :float])

(defpacket ::block-change {:state :play :dir :s->c :id 0x23}
  [:location    :position]
  [:block-state :varint])

(spec/register!
 {:key ::multi-block-change :state :play :dir :s->c :id 0x22
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-int buf (:chunk-x m))
           (t/write-int buf (:chunk-z m))
           (let [records (:records m)]
             (t/write-varint buf (count records))
             (doseq [[lx y lz state] records]
               (t/write-short buf (bit-or (bit-shift-left (long lx) 12)
                                          (bit-shift-left (long lz) 8)
                                          (long y)))
               (t/write-varint buf state))))})

(defpacket ::effect {:state :play :dir :s->c :id 0x28}
  [:effect-id :int]
  [:location  :position]
  [:data      :int]
  [:global?   :bool])

(defpacket ::sound-effect {:state :play :dir :s->c :id 0x29}
  [:name   :string]
  [:x      :int]
  [:y      :int]
  [:z      :int]
  [:volume :float]
  [:pitch  :ubyte])

(spec/register!
 {:key ::player-list-item :state :play :dir :s->c :id 0x38
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (let [action  (long (:action m))
                 entries (:entries m)]
             (t/write-varint buf action)
             (t/write-varint buf (count entries))
             (doseq [e entries]
               (t/write-uuid buf (:uuid e))
               (case action
                 0 (do (t/write-string buf (:name e))
                       (t/write-varint buf 0)
                       (t/write-varint buf (:gamemode e))
                       (t/write-varint buf (:ping e))
                       (t/write-bool buf false))
                 2 (t/write-varint buf (:ping e))
                 4 nil))))})

(def ^:const list-add     0)
(def ^:const list-latency 2)
(def ^:const list-remove  4)
(defpacket ::spawn-player {:state :play :dir :s->c :id 0x0C}
  [:entity-id    :varint]
  [:uuid         :uuid]
  [:x            :int]
  [:y            :int]
  [:z            :int]
  [:yaw          :byte]
  [:pitch        :byte]
  [:current-item :short]
  [:metadata     :metadata])

(spec/register!
 {:key ::destroy-entities :state :play :dir :s->c :id 0x13
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (let [ids (:entity-ids m)]
             (t/write-varint buf (count ids))
             (doseq [id ids] (t/write-varint buf id))))})

(defpacket ::entity-rel-move {:state :play :dir :s->c :id 0x15}
  [:entity-id :varint]
  [:dx :byte] [:dy :byte] [:dz :byte]
  [:on-ground :bool])

(defpacket ::entity-look {:state :play :dir :s->c :id 0x16}
  [:entity-id :varint]
  [:yaw :byte] [:pitch :byte]
  [:on-ground :bool])

(defpacket ::entity-look-move {:state :play :dir :s->c :id 0x17}
  [:entity-id :varint]
  [:dx :byte] [:dy :byte] [:dz :byte]
  [:yaw :byte] [:pitch :byte]
  [:on-ground :bool])

(defpacket ::entity-teleport {:state :play :dir :s->c :id 0x18}
  [:entity-id :varint]
  [:x :int] [:y :int] [:z :int]
  [:yaw :byte] [:pitch :byte]
  [:on-ground :bool])

(defpacket ::entity-head-look {:state :play :dir :s->c :id 0x19}
  [:entity-id :varint]
  [:yaw :byte])

(defpacket ::entity-metadata {:state :play :dir :s->c :id 0x1C}
  [:entity-id :varint]
  [:metadata  :metadata])

(defpacket ::animation {:state :play :dir :s->c :id 0x0B}
  [:entity-id :varint]
  [:animation :ubyte])

(defpacket ::keep-alive-c {:state :play :dir :c->s :id 0x00}
  [:id :varint])

(defpacket ::chat-c {:state :play :dir :c->s :id 0x01}
  [:message :string])

(defpacket ::player {:state :play :dir :c->s :id 0x03}
  [:on-ground :bool])

(defpacket ::player-position {:state :play :dir :c->s :id 0x04}
  [:x :double] [:y :double] [:z :double]
  [:on-ground :bool])

(defpacket ::player-look {:state :play :dir :c->s :id 0x05}
  [:yaw :float] [:pitch :float]
  [:on-ground :bool])

(defpacket ::player-position-look {:state :play :dir :c->s :id 0x06}
  [:x :double] [:y :double] [:z :double]
  [:yaw :float] [:pitch :float]
  [:on-ground :bool])

(defpacket ::animation-c {:state :play :dir :c->s :id 0x0A})
(defpacket ::entity-action {:state :play :dir :c->s :id 0x0B}
  [:entity-id :varint]
  [:action    :varint]
  [:aux       :varint])

(defpacket ::client-status {:state :play :dir :c->s :id 0x16}
  [:action :varint])

(defpacket ::client-settings {:state :play :dir :c->s :id 0x15}
  [:locale         :string]
  [:view-distance  :byte]
  [:chat-mode      :byte]
  [:chat-colors    :bool]
  [:skin-parts     :ubyte])

(spec/register!
 {:key ::use-entity :state :play :dir :c->s :id 0x02
  :read (fn [^ByteBuf buf]
          {:packet/key ::use-entity
           :target (t/read-varint buf)
           :action (t/read-varint buf)})
  :write (fn [_ _] (throw (UnsupportedOperationException. "c->s only")))})

(spec/register!
 {:key ::tab-complete-c :state :play :dir :c->s :id 0x14
  :read (fn [^ByteBuf buf]
          (let [message (t/read-string buf)
                target? (t/read-bool buf)]
            {:packet/key ::tab-complete-c
             :message message
             :target   (when target? (t/read-position buf))}))
  :write (fn [_ _] (throw (UnsupportedOperationException. "c->s only")))})

(spec/register!
 {:key ::tab-complete :state :play :dir :s->c :id 0x3A
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (let [ms (:matches m)]
             (t/write-varint buf (count ms))
             (doseq [s ms] (t/write-string buf (str s)))))})

(defn tab-complete [matches]
  {:packet/key ::tab-complete :matches (vec matches)})

(defpacket ::player-digging {:state :play :dir :c->s :id 0x07}
  [:status   :byte]
  [:location :position]
  [:face     :byte])

(defpacket ::held-item-change {:state :play :dir :c->s :id 0x09}
  [:slot :short])

(spec/register!
 {:key ::creative-inventory-action :state :play :dir :c->s :id 0x10
  :read (fn [^ByteBuf buf]
          (let [slot (t/read-short buf)
                item (t/read-short buf)]
            (merge {:packet/key ::creative-inventory-action :slot slot}
                   (when-not (neg? item)
                     {:stack {:item item
                              :count (t/read-byte buf)
                              :damage (t/read-short buf)}}))))
  :write (fn [_ _] (throw (UnsupportedOperationException. "c->s only")))})

(spec/register!
 {:key ::entity-equipment :state :play :dir :s->c :id 0x04
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-varint buf (:entity-id m))
           (t/write-short buf (:slot m))
           (t/write-slot buf (:stack m)))})

(spec/register!
 {:key ::window-items :state :play :dir :s->c :id 0x30
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-ubyte buf (:window-id m))
           (let [slots (:slots m)]
             (t/write-short buf (count slots))
             (doseq [s slots] (t/write-slot buf s))))})

(spec/register!
 {:key ::set-slot :state :play :dir :s->c :id 0x2F
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-byte buf (:window-id m))
           (t/write-short buf (:slot m))
           (t/write-slot buf (:stack m)))})

(defn set-slot [slot stack]
  {:packet/key ::set-slot :window-id 0 :slot slot :stack stack})

(defpacket ::collect-item {:state :play :dir :s->c :id 0x0D}
  [:collected-id :varint]
  [:collector-id :varint])

(spec/register!
 {:key ::spawn-object :state :play :dir :s->c :id 0x0E
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-varint buf (:entity-id m))
           (t/write-byte buf (:object-type m))
           (t/write-int buf (:x m))
           (t/write-int buf (:y m))
           (t/write-int buf (:z m))
           (t/write-byte buf (:pitch m))
           (t/write-byte buf (:yaw m))
           (t/write-int buf (:data m))
           (when (pos? (long (:data m)))
             (t/write-short buf (:vx m))
             (t/write-short buf (:vy m))
             (t/write-short buf (:vz m))))})

(defn- velocity-short ^long [v]
  (-> (long (* (double v) 8000.0)) (max -32767) (min 32767)))

(defn spawn-item [eid [x y z] [vx vy vz]]
  {:packet/key ::spawn-object
   :entity-id eid :object-type 2
   :x x :y y :z z
   :pitch 0 :yaw 0 :data 1
   :vx (velocity-short vx) :vy (velocity-short vy) :vz (velocity-short vz)})

(defn collect-item [item-eid collector-eid]
  {:packet/key ::collect-item :collected-id item-eid :collector-id collector-eid})

(defn spawn-tnt [eid [x y z]]
  {:packet/key ::spawn-object
   :entity-id eid :object-type 50
   :x x :y y :z z
   :pitch 0 :yaw 0 :data 0})

(spec/register!
 {:key ::explosion :state :play :dir :s->c :id 0x27
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (let [x (double (:x m)) y (double (:y m)) z (double (:z m))]
             (.writeFloat buf (float x))
             (.writeFloat buf (float y))
             (.writeFloat buf (float z))
             (.writeFloat buf (float (:radius m)))
             (.writeInt buf (count (:records m)))
             (doseq [[bx by bz] (:records m)]
               (.writeByte buf (int (- (long bx) (long x))))
               (.writeByte buf (int (- (long by) (long y))))
               (.writeByte buf (int (- (long bz) (long z)))))
             (let [[mx my mz] (:motion m [0.0 0.0 0.0])]
               (.writeFloat buf (float mx))
               (.writeFloat buf (float my))
               (.writeFloat buf (float mz)))))})

(defn explosion [[x y z] radius records motion]
  {:packet/key ::explosion
   :x x :y y :z z :radius radius :records records :motion motion})

(spec/register!
 {:key ::spawn-mob :state :play :dir :s->c :id 0x0F
  :read (fn [_] (throw (UnsupportedOperationException. "s->c only")))
  :write (fn [^ByteBuf buf m]
           (t/write-varint buf (:entity-id m))
           (t/write-ubyte buf (:mob-type m))
           (t/write-int buf (:x m))
           (t/write-int buf (:y m))
           (t/write-int buf (:z m))
           (t/write-byte buf (:yaw m))
           (t/write-byte buf (:pitch m))
           (t/write-byte buf (:head m))
           (t/write-short buf (:vx m))
           (t/write-short buf (:vy m))
           (t/write-short buf (:vz m))
           (t/write-metadata buf (:metadata m)))})

(defn spawn-mob [eid mob-type [x y z] yaw pitch metadata]
  {:packet/key ::spawn-mob
   :entity-id eid :mob-type mob-type
   :x x :y y :z z
   :yaw yaw :pitch pitch :head yaw
   :vx 0 :vy 0 :vz 0
   :metadata metadata})

(defpacket ::entity-status {:state :play :dir :s->c :id 0x1A}
  [:entity-id :int]
  [:status    :byte])

(defn entity-status [eid status]
  {:packet/key ::entity-status :entity-id eid :status status})

(defpacket ::entity-velocity {:state :play :dir :s->c :id 0x12}
  [:entity-id :varint]
  [:vx :short] [:vy :short] [:vz :short])

(defn entity-velocity [eid [vx vy vz]]
  {:packet/key ::entity-velocity :entity-id eid
   :vx (velocity-short vx) :vy (velocity-short vy) :vz (velocity-short vz)})

(defn equipment [eid slot stack]
  {:packet/key ::entity-equipment :entity-id eid :slot slot :stack stack})

(defn window-items [slots]
  {:packet/key ::window-items :window-id 0 :slots slots})

(defn block-change [pos state]
  {:packet/key ::block-change :location pos :block-state state})

(defn multi-block-change [[cx cz] changes]
  {:packet/key ::multi-block-change
   :chunk-x cx
   :chunk-z cz
   :records (mapv (fn [[[x y z] st]]
                    [(bit-and (long x) 15) y (bit-and (long z) 15) st])
                  changes)})

(defn break-effect [pos state]
  {:packet/key ::effect
   :effect-id 2001 :location pos
   :data (bit-or (bit-shift-right (long state) 4)
                 (bit-shift-left (bit-and (long state) 15) 12))
   :global? false})

(defn fizz-effect [pos]
  {:packet/key ::effect :effect-id 1004 :location pos :data 0 :global? false})

(defn entity-sound [name [x y z] volume pitch]
  {:packet/key ::sound-effect
   :name name
   :x (long (* (double x) 8.0))
   :y (long (* (double y) 8.0))
   :z (long (* (double z) 8.0))
   :volume volume
   :pitch pitch})

(defn named-sound [name [x y z] volume pitch]
  {:packet/key ::sound-effect
   :name name
   :x (+ (* (long x) 8) 4)
   :y (+ (* (long y) 8) 4)
   :z (+ (* (long z) 8) 4)
   :volume volume
   :pitch pitch})

(spec/register!
 {:key ::player-block-placement :state :play :dir :c->s :id 0x08
  :read (fn [^ByteBuf buf]
          (let [loc     (t/read-position buf)
                face    (t/read-byte buf)
                item-id (t/read-short buf)
                damage  (if (neg? item-id)
                          0
                          (do (t/read-ubyte buf) (t/read-short buf)))
                wi      (.writerIndex buf)]
            {:packet/key ::player-block-placement
             :location loc :face face :item-id item-id :damage damage
             :cursor [(long (.getByte buf (- wi 3)))
                      (long (.getByte buf (- wi 2)))
                      (long (.getByte buf (- wi 1)))]}))
  :write (fn [_ _] (throw (UnsupportedOperationException. "c->s only")))})
