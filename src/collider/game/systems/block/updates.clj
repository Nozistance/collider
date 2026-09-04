(ns collider.game.systems.block.updates
  (:require [clojure.data.int-map :as i]
            [collider.game.state :as state]
            [collider.game.tnt :as tnt]
            [collider.game.out :as out]
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
             (and (chunk/in-range? ny)
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

(defn- block-flush-deltas [world]
  (when-let [events (:block-events world)]
    (cons [:block-events-flushed]
          (mapcat (fn [[cp recs]]
                    [(out/all (out/blocks-changed cp recs))])
                  events))))

(defn block-flush [world _events]
  [#(block-flush-deltas world)])

(defn- fizz-deltas [world changes]
  (for [[pos st] changes
        :let [old (chunk/chunks-get-block (:chunks world) gen/flat-chunk pos)]
        :when (or (and (pos? (long st))
                       (nil? (liquid/liquid-class st))
                       (or (liquid/liquid-state? old) (zero? (long old))))
                  (and (liquid/mix-class? st) (pos? (long old))))
        d [(out/all (out/fizz pos))]]
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
                    [(out/all (out/sound :tnt/primed pos 1.0 1.0))]))
            tnts)))

(defn- block-updates-deltas [world _events]
  (let [t   (long (:tick world))
        due (into (i/int-set) (comp (take-while (fn [[k _]] (<= (long k) t))) (mapcat val))
                  (:block-ticks world))]
    (when (seq due)
      (let [active  (state/active-chunks world)
            now     (into [] (comp (filter #(state/active-id? active %))
                                   (map chunk/id->block-pos)) due)
            parked  (into [] (remove #(state/active-id? active %)) due)
            changes (lww-changes (:chunks world) now)]
        (concat
         [[:ticks-flushed t parked]]
         (when (seq changes)
           (concat [[:set-blocks changes]]
                   (fizz-deltas world changes)))
         (ignite-deltas world now))))))

(defn block-updates [world events]
  [#(block-updates-deltas world events)])
