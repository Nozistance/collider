(ns collider.game.block.instrument
  "The note a note block plays."
  (:require [collider.data :as data]
            [collider.game.out :as out]
            [collider.num :as num]
            [collider.world.block :as block]
            [collider.world.env.signal :as signal]))

(set! *warn-on-reflection* true)

(defn- pitch ^double [^long note]
  (num/f32 (Math/pow 2.0 (/ (- note 12) 12.0))))

(defn played-deltas
  "Returns the deltas of the note block st at pos asked to play, which
  entity source set off, nil for none. The note sounds when its block
  event runs."
  [pos ^long st source]
  (into [[:block-event-queued pos (block/block-of st) 0 0]]
        (signal/game-event :note-block-play pos source)))

(defn sounded
  "Returns the deltas of the block event of the note block st at pos,
  nil when it takes none."
  [pos ^long st]
  (let [props (block/props-of st)
        {:keys [tunable? custom? sound]}
        (get (data/instruments) (:instrument props))
        p (if tunable? (pitch (block/prop-long st :note)) 1.0)]
    (when-not custom?
      [(out/all (out/block-sound sound pos 3.0 p :records))])))
