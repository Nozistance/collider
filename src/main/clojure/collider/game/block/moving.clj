(ns collider.game.block.moving
  "The record of a block a piston is moving."
  (:require [collider.world.block :as block]
            [collider.world.blocks.piston :as piston]
            [collider.world.direction :as dir]))

(set! *warn-on-reflection* true)

(defn- head-short? [e]
  (let [p (/ (double (long (:progress e))) 2.0)]
    (not= (boolean (:extending? e)) (< (- 1.0 p) 0.25))))

(defn- collision-head ^long [e]
  (block/state :piston-head
               {:facing (:facing e)
                :short (if (head-short? e) :true :false)
                :type :normal}))

(defn- shape-state ^long [e]
  (if (:source? e) (collision-head e) (long (:moved e))))

(defn- base-state ^long [e]
  (let [st (long (:moved e))]
    (if (and (not (:extending? e)) (:source? e) (piston/base? st))
      (block/with st :extended :true)
      0)))

(defn- check-state ^long [^long st]
  (when-not (and (<= 0 st) (< st (bit-shift-left 1 20)))
    (throw (ex-info "a moving state does not fit" {:state st})))
  st)

(defn- fits-progress [e]
  (let [p (long (:progress e))
        f (long (dir/index (:facing e)))]
    (when-not (< -1 p 4)
      (throw (ex-info "a moving progress does not fit"
                      {:progress p})))
    (when-not (< -1 f 6)
      (throw (ex-info "a moving facing does not fit" {:facing f})))
    [p f]))

(defn packed
  "Returns the moving record of piston block entity e.
  A state, a facing or a progress that does not fit throws."
  ^long [e]
  (let [shape (check-state (shape-state e))
        base (check-state (base-state e))
        [progress facing] (fits-progress e)]
    (bit-or (long shape)
            (bit-shift-left (long base) 20)
            (bit-shift-left (long facing) 40)
            (bit-shift-left (long progress) 43)
            (if (:extending? e) (bit-shift-left 1 45) 0))))
