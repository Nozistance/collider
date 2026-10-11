(ns collider.game.block.lock
  "The locks of containers. A lock is an item predicate as a tag
  holds it, which the main hand must pass."
  (:require [collider.game.item-predicate :as ip]))

(set! *warn-on-reflection* true)

(defn unlocks?
  "Returns true when stack s, nil for an empty hand, opens lock. No
  lock, or one that does not decode, opens with anything."
  [lock s]
  (let [[op p] (ip/decode (or lock {}))]
    (or (not= :ok op) (ip/matches? p s))))
