(ns collider.cell
  "Block cells packed into one long."
  (:import (collider Cell)))

(set! *warn-on-reflection* true)

(defn pack
  {:inline (fn [x y z] `(Cell/pack ~x ~y ~z))
   :inline-arities #{3}}
  (^long [[x y z]] (Cell/pack (long x) (long y) (long z)))
  (^long [^long x ^long y ^long z] (Cell/pack x y z)))

(defn x {:inline (fn [c] `(Cell/x ~c))} ^long [^long c] (Cell/x c))

(defn y {:inline (fn [c] `(Cell/y ~c))} ^long [^long c] (Cell/y c))

(defn z {:inline (fn [c] `(Cell/z ~c))} ^long [^long c] (Cell/z c))

(defn offset
  {:inline (fn [c dx dy dz] `(Cell/offset ~c ~dx ~dy ~dz))}
  ^long [^long c ^long dx ^long dy ^long dz]
  (Cell/offset c dx dy dz))

(defn unpack
  "Returns the coordinates of cell c as [x y z]."
  [^long c]
  [(Cell/x c) (Cell/y c) (Cell/z c)])
