(ns collider.world.chunk.batch
  "The buffer that gathers the block edits of one section."
  (:import (java.util Arrays)))

(set! *warn-on-reflection* true)

(definterface Edits
  (add [^long i ^long state])
  (applyTo [^collider.java.Section s]))

(deftype Batch
  [^:unsynchronized-mutable ^ints idx
   ^:unsynchronized-mutable ^ints states
   ^:unsynchronized-mutable ^long n]
  Edits
  (add [_ i state]
    (when (= n (alength idx))
      (set! idx (Arrays/copyOf idx (int (* 2 n))))
      (set! states (Arrays/copyOf states (int (* 2 n)))))
    (aset idx n (int i))
    (aset states n (int state))
    (set! n (inc n)))
  (applyTo [_ s]
    (.apply s idx states (int n))))
