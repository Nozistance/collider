(ns collider.render
  "Packets of one tick. `render` takes the world and the deltas and returns
   [eid packet] pairs. Chunk and entity packets come from :chunks-sent and
   :tracking deltas, everything else from effects."
  (:require [clojure.data.int-map :as i]
            [collider.data :as data]
            [collider.game.deltas]
            [collider.game.rules :as rules]
            [collider.log :as log]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen])
  (:import (collider.game.deltas Deltas)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- players [world]
  (sort (vals (:players world))))

(defn- text-of
  "Component of the runs: one translatable run goes as is, plain runs join."
  [runs]
  (if-let [t (some :translate runs)]
    (select-keys (first (filter :translate runs)) [:translate :with])
    (apply str (map :text runs))))

(defn- chunk-packets [world [_ eid cp add drop]]
  (let [[cx cz] (chunk/id->pos cp)]
    (concat
     [{:packet :set-chunk-cache-center :cx cx :cz cz}]
     (when (seq add)
       (concat
        [{:packet :chunk-batch-start}]
        (map (fn [id]
               (let [[x z] (chunk/id->pos id)]
                 {:packet :level-chunk-with-light :cx x :cz z
                  :chunk (get-in world [:chunks id] gen/flat-chunk)}))
             add)
        [{:packet :chunk-batch-finished :size (count add)}]))
     (map (fn [id]
            (let [[x z] (chunk/id->pos id)]
              {:packet :forget-level-chunk :cx x :cz z}))
          drop))))

(def ^:private entity-type
  (delay {:player (data/registry-id "entity_type" :player)
          :sheep  (data/registry-id "entity_type" :sheep)
          :item   (data/registry-id "entity_type" :item)
          :tnt    (data/registry-id "entity_type" :tnt)}))

(defn- kind-of [e]
  (let [t (:type e)]
    (if (contains? @entity-type t) t :player)))

(defn- uuid-of [eid e]
  (or (:uuid e) (UUID. (long eid) (long eid))))

(defn- flags-byte [meta]
  (bit-or (if (:burning? meta) 0x01 0)
          (if (:sneaking? meta) 0x02 0)
          (if (:sprinting? meta) 0x08 0)))

(defn- entity-data [kind meta]
  (case kind
    :player [[0 :byte (flags-byte meta)]
             [6 :pose (if (:sleeping-pos meta) 2 0)]
             [8 :byte (if (:using-item? meta) 0x01 0)]
             [14 :optional-block-pos (:sleeping-pos meta)]]
    :sheep  [[0 :byte (flags-byte meta)]
             [16 :boolean (boolean (:baby? meta))]
             [18 :byte (bit-and (long (or (:color meta) 0)) 15)]]
    :item   [[8 :item (:stack meta)]]
    :tnt    [[8 :int 80]]
    []))

(def ^:private equipment-slots [0 2 3 4 5])

(defn- spawn-packets [world eid]
  (when-let [e (get-in world [:entities eid])]
    (let [kind  (kind-of e)
          tr    (:track e)
          meta  (if tr (:mdata tr) {})
          equip (keep-indexed (fn [i s] (when s [(equipment-slots i) s])) (if tr (:equip tr) []))
          d     (entity-data kind meta)]
      (concat
       [{:packet :bundle-delimiter}
        ;; the position and velocity the track started from: the relative
        ;; moves that follow are counted from there, so the client must
        ;; start its own simulation there too
        {:packet :add-entity :eid eid :uuid (uuid-of eid e) :type (@entity-type kind)
         :pos (if tr (mapv #(/ (double %) 4096.0) (:pos tr)) (:pos e))
         :vel (or (when tr (:vel-sent tr)) (:vel e) [0.0 0.0 0.0])
         :yaw (:yaw e 0.0) :pitch (:pitch e 0.0) :head-yaw (or (:head-yaw e) (:yaw e 0.0))}]
       (when (seq d) [{:packet :set-entity-data :eid eid :data d}])
       (when (seq equip) [{:packet :set-equipment :eid eid :slots equip}])
       [{:packet :bundle-delimiter}]))))

(defn- tracking-packets [world [_ _ add gone]]
  (concat
   (mapcat #(spawn-packets world %) add)
   (when (seq gone) [{:packet :remove-entities :eids gone}])))

(def ^:private sounds
  (delay
   (into {}
         (map (fn [[k [ev src]]] [k [(data/registry-id "sound_event" ev) src]]))
         {:player/hurt         [:entity.player.hurt 7]
          :player/hurt-on-fire [:entity.player.hurt-on-fire 7]
          :player/death        [:entity.player.death 7]
          :sheep/say    [:entity.sheep.ambient 6]
          :sheep/step   [:entity.sheep.step 6]
          :tnt/primed   [:entity.tnt.primed 4]
          :fire/ignite  [:item.flintandsteel.use 4]
          :explosion    [:entity.generic.explode 4]
          :splash       [:entity.generic.splash 6]
          :swim         [:entity.generic.swim 6]
          :place/stone  [:block.stone.place 4]
          :place/wood   [:block.wood.place 4]
          :place/grass  [:block.grass.place 4]
          :place/gravel [:block.gravel.place 4]
          :place/sand   [:block.sand.place 4]
          :place/cloth  [:block.wool.place 4]
          :place/glass  [:block.glass.place 4]
          :place/snow   [:block.snow.place 4]})))

(def ^:private overworld (delay (data/datapack-id "dimension_type" :overworld)))
(def ^:private explosion-particle (delay (data/registry-id "particle_type" :explosion-emitter)))

(defn- particles-packet [m]
  {:packet :level-particles :particle (data/registry-id "particle_type" (:kind m)) :state (:state m)
   :pos (:pos m) :count (:count m) :speed (:speed m)})

(def ^:private unhandled (atom #{}))

(defn- once! [kind]
  (when-not (@unhandled kind)
    (swap! unhandled conj kind)
    (log/info "render:" kind "not rendered yet")))

(defn- block-records [[cx cz] records]
  (for [[sy recs] (group-by (fn [[[_ y _] _]] (bit-shift-right (long y) 4)) records)]
    {:packet :section-blocks-update :section [cx sy cz]
     :changes (map (fn [[[x y z] st]]
                     [(bit-or (bit-shift-left (bit-and (long x) 15) 8)
                              (bit-shift-left (bit-and (long z) 15) 4)
                              (bit-and (long y) 15))
                      st])
                   recs)}))

(defn- status-packet [m]
  (case (:kind m)
    :hurt  {:packet :hurt-animation :eid (:eid m) :yaw 0.0}
    :death {:packet :entity-event :eid (:eid m) :event 3}
    :eat   {:packet :entity-event :eid (:eid m) :event 10}
    :love  {:packet :entity-event :eid (:eid m) :event 18}
    nil))

(defn- sound-id [kind]
  (or (get @sounds kind)
      (when-let [id (get (get @data/registries "sound_event") kind)] [id 4])))

(defn- sound-packet [m]
  (if-let [[id src] (sound-id (:kind m))]
    {:packet :sound :sound id :source src :pos (:pos m) :volume (:volume m) :pitch (:pitch m)}
    (once! [:sound (:kind m)])))

(defn- explode-packet [m]
  (let [k (:motion m)]
    {:packet :explode :center (:center m) :radius (:radius m) :blocks (:blocks m)
     :knockback (when (and k (some #(not (zero? (double %))) k)) k)
     :particle @explosion-particle :sound (first (get @sounds :explosion))}))

(defn- fx-packets [world m]
  (case (:msg m)
    :teleport   [{:packet :player-position :teleport-id (long (:tick world))
                  :pos (:pos m) :yaw (:yaw m) :pitch (:pitch m)}]
    :keepalive  [{:packet :keep-alive :id (:id m)}]
    :disconnect [{:packet :disconnect :text (:text m)}]
    :system-chat [{:packet :system-chat :text (text-of (:runs m)) :overlay false}]
    :overlay    [{:packet :system-chat :text (text-of (:runs m)) :overlay true}]
    :stats      [{:packet :award-stats :stats (:stats m)}]
    :suggestions [{:packet :command-suggestions :id (:id m) :start (:start m) :length (:length m) :matches (:matches m)}]
    :game-rules [{:packet :game-rule-values
                  :values (map (fn [[k v]] [(rules/wire-name k) (rules/serialize k v)]) (:rules m))}]
    :player-chat [{:packet :system-chat :text (str "<" (:name m) "> " (text-of (:runs m))) :overlay false}]
    :health     [{:packet :set-health :health (:health m) :food 20 :saturation 5.0}]
    ;; после Respawn клиент ждёт game-event 13 (LEVEL_CHUNKS_LOAD_START),
    ;; иначе висит на «Loading terrain» до таймаута; ваниль шлёт его в sendLevelInfo
    :respawn    [{:packet :respawn :dimension-type @overworld :keep 0}
                 {:packet :game-event :event 13 :value 0.0}]
    :time       [{:packet :set-time :age (:age m) :time (:time m)}]
    :block-change [{:packet :block-update :pos (:pos m) :state (:state m)}]
    :blocks-changed (block-records (chunk/id->pos (:cp m)) (:records m))
    :set-slot   [{:packet :container-set-slot :slot (:slot m) :stack (:stack m)}]
    :carried    [{:packet :container-set-slot :container -1 :slot -1 :stack (:stack m)}]
    :held-slot  [{:packet :set-held-slot :slot (:slot m)}]
    :block-ack  [{:packet :block-changed-ack :sequence (:sequence m)}]
    :inventory  [{:packet :container-set-content :items (:slots m) :carried (:carried m)}]
    :tab-add    [{:packet :player-info-update :players (:entries m)}]
    :tab-remove [{:packet :player-info-remove :uuids (:uuids m)}]
    :tab-latency [{:packet :player-info-update :action :latency :players (:entries m)}]
    :tab-header [{:packet :tab-list :header (:header m) :footer (:footer m)}]
    :break-effect [{:packet :level-event :event 2001 :pos (:pos m) :data (:state m)}]
    :fizz [{:packet :level-event :event 1501 :pos (:pos m) :data 0}]
    :extinguish [{:packet :level-event :event 1009 :pos (:pos m) :data 0}]
    :move [{:packet :move-entity-pos :eid (:eid m) :dx (:dx m) :dy (:dy m) :dz (:dz m) :on-ground (:on-ground m)}]
    :move-look [{:packet :move-entity-pos-rot :eid (:eid m) :dx (:dx m) :dy (:dy m) :dz (:dz m)
                 :yaw (:yaw m) :pitch (:pitch m) :on-ground (:on-ground m)}]
    :look [{:packet :move-entity-rot :eid (:eid m) :yaw (:yaw m) :pitch (:pitch m) :on-ground (:on-ground m)}]
    :sync-pos [{:packet :entity-position-sync :eid (:eid m) :pos (:pos m) :yaw (:yaw m) :pitch (:pitch m)
                :on-ground (:on-ground m)}]
    :head-look [{:packet :rotate-head :eid (:eid m) :yaw (:yaw m)}]
    :velocity [{:packet :set-entity-motion :eid (:eid m) :vel (:vel m)}]
    :meta (let [d (entity-data (kind-of (get-in world [:entities (:eid m)])) (:meta m))]
            (when (seq d) [{:packet :set-entity-data :eid (:eid m) :data d}]))
    :equipment [{:packet :set-equipment :eid (:eid m) :slots [[(equipment-slots (:slot m)) (:stack m)]]}]
    :animation [{:packet :animate :eid (:eid m) :action (case (:kind m) :swing 0 :wake-up 2 :crit 4 0)}]
    :status (when-let [p (status-packet m)] [p])
    :collect [{:packet :take-item-entity :item (:item m) :collector (:collector m) :amount 1}]
    :sound (when-let [p (sound-packet m)] [p])
    :particles [(particles-packet m)]
    :explosion [(explode-packet m)]
    :close      [:close]
    (once! (:msg m))))

(defn- viewer-index [world]
  (persistent!
   (reduce (fn [acc pid]
             (reduce (fn [a eid] (assoc! a eid (conj (get a eid []) pid)))
                     acc
                     (seq (get-in world [:entities pid :tracking]))))
           (transient (i/int-map))
           (players world))))

(def ^:private entity-msgs
  #{:move :move-look :look :sync-pos :head-look :velocity :meta :equipment :animation :status :collect})

(defn- recipients [world viewers m]
  (cond
    (:to m)     [(:to m)]
    (:except m) (remove #{(:except m)} (players world))
    (entity-msgs (:msg m)) (get @viewers (long (:eid m)) [])
    :else       (players world)))

(defn render
  "Packets of one tick as [eid packet] pairs, in send order per player."
  [world ^Deltas deltas]
  (let [viewers (delay (viewer-index world))]
    (concat
     (for [[eid ds] (.entities deltas)
           d ds
           p (case (first d)
               :chunks-sent (chunk-packets world d)
               :tracking (tracking-packets world d)
               nil)]
       [eid p])
     (for [m (.out deltas)
           :let [ps (fx-packets world m)]
           :when (seq ps)
           eid (recipients world viewers m)
           p ps]
       [eid p]))))
