(ns collider.game.systems.block.updates
  (:require [collider.game.state :as state]
            [collider.game.tnt :as tnt]
            [collider.proto.packets.play :as play]
            [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]
            [collider.world.fire :as fire]
            [collider.world.liquid :as liquid]
            [collider.world.rules :as rules]))

(set! *warn-on-reflection* true)

(def ^:private sides
  [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn- tnt-neighbors [chunks [x y z]]
  (filterv (fn [[_ ny _ :as p]]
             (and (<= 0 (long ny) 255)
                  (tnt/tnt-state? (chunk/chunks-get-block chunks gen/flat-chunk p))))
           (map (fn [d] (mapv + [x y z] d)) sides)))

(defn- lww-changes [chunks cells]
  (into []
        (vals (into (sorted-map)
                    (map (fn [[pos st]] [pos [pos st]]))
                    (mapcat (fn [p] (rules/cell-changes
                                      chunks
                                      (chunk/chunks-get-block chunks gen/flat-chunk p)
                                      p))
                                    cells)))))

(defn- sections-packet [world cp sis]
  (let [[cx cz] (chunk/id->pos cp)
        c (get-in world [:chunks cp])
        [bm data] (chunk/encode-sections (or c gen/flat-chunk) sis)]
    {:packet/key ::play/chunk-data :chunk-x cx :chunk-z cz
     :ground-up? false :bitmask bm :data data}))

(defn- chunk-resend-deltas [world cp sis]
  (into [] (keep (fn [[eid e]]
                   (when (and (= :player (:type e))
                              (contains? (or (:sent-chunks e) #{}) cp))
                     [:send eid (sections-packet world cp sis)])))
        (:entities world)))

(def ^:private ^:const resend-cooldown 20)
(defn- block-flush-deltas [world]
  (let [t      (long (:tick world))
        sent   (:chunk-resent world)
        cooled? (fn [cp] (>= (- t (long (get sent cp -1000))) resend-cooldown))
        events (:block-events world)
        small  (when events (filterv (fn [[_ recs]] (<= (count recs) 64)) events))
        big    (reduce (fn [m [cp recs]]
                         (if (> (count recs) 64)
                           (update m cp (fnil into #{})
                                   (map (fn [[[_ y _] _]] (bit-shift-right (long y) 4))) recs)
                           m))
                       (or (:dirty-chunks world) {})
                       events)
        now    (into {} (filter (fn [[cp _]] (cooled? cp))) big)
        held   (into {} (remove (fn [[cp _]] (cooled? cp))) big)]
    (concat
     (when events [[:block-events-flushed]])
     (when (or (seq big) (seq (:dirty-chunks world)))
       [[:chunk-flush held (vec (keys now)) t]])
     (mapcat (fn [[cp recs]]
               (state/broadcast world (play/multi-block-change (chunk/id->pos cp) recs)))
             small)
     (mapcat (fn [[cp sis]] (chunk-resend-deltas world cp sis)) now))))

(defn block-flush [world _events]
  [#(block-flush-deltas world)])

(defn- fizz-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
        :when (or (and (pos? (long st))
                       (not (liquid/liquid-state? st))
                       (or (liquid/liquid-state? old) (zero? (long old))))
                  (and (liquid/mix-class? st) (pos? (long old))))
        d (state/broadcast world (play/fizz-effect pos))]
    d))

(defn- ignite-deltas [world due]
  (let [chunks   (:chunks world)
        pending  (tnt/primed-origins world)
        tnts     (into (sorted-set)
                       (comp (filter #(fire/fire-state?
                                       (chunk/chunks-get-block chunks gen/flat-chunk %)))
                             (mapcat #(tnt-neighbors chunks %))
                             (remove pending))
                       due)]
    (mapcat (fn [pos]
              (cons [:spawn-entity (tnt/primed pos [(:tick world) pos])]
                    (state/broadcast world (play/named-sound "game.tnt.primed"
                                                             pos 1.0 63))))
            tnts)))

(defn- block-updates-deltas [world _events]
  (let [t   (long (:tick world))
        due (get (:block-ticks world) t)]
    (when (seq due)
      (let [active  (state/active-chunks world)
            now     (into [] (comp (filter #(state/active-id? active %))
                                   (map chunk/id->block-pos)) due)
            parked  (into [] (remove #(state/active-id? active %)) due)
            changes (lww-changes (:chunks world) now)]
        (concat
         [[:ticks-flushed t [] parked]]
         (when (seq changes)
           (concat [[:set-blocks changes]]
                   (fizz-deltas world changes)))
         (ignite-deltas world now))))))

(defn block-updates [world events]
  [#(block-updates-deltas world events)])
