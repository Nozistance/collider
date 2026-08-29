(ns collider.world.support
  (:require [collider.world.chunk :as chunk]
            [collider.world.gen :as gen]))

(set! *warn-on-reflection* true)

(def ^:private torch-ids  #{50 75 76})
(def ^:private side-ids   #{65 68})
(def ^:private ground-ids #{6 27 28 31 32 37 38 39 40 51 55 59 63 66 70 72 78 104 105 141 142 171})
(def ^:private ^booleans fragile-table
  (let [a (boolean-array 256)]
    (doseq [id (concat torch-ids side-ids ground-ids)] (aset a (long id) true))
    a))

(defn fragile-id? [id]
  (let [id (long id)]
    (and (< -1 id 256) (aget ^booleans fragile-table id))))

(defn fragile? [st] (fragile-id? (bit-shift-right (long st) 4)))
(def ^:private washable-ids (disj (into torch-ids ground-ids) 63))

(defn washable? [st] (contains? washable-ids (bit-shift-right (long st) 4)))
(def ^:private torch-support
  {1 [-1 0 0], 2 [1 0 0], 3 [0 0 -1], 4 [0 0 1]})

(def ^:private side-support
  {2 [0 0 1], 3 [0 0 -1], 4 [1 0 0], 5 [-1 0 0]})

(defn- state-at ^long [chunks template [_ y _ :as pos]]
  (let [y (long y)]
    (if (or (neg? y) (> y 255))
      -1
      (chunk/chunks-get-block chunks template pos))))

(defn- solid-at? [chunks template pos]
  (let [st (state-at chunks template pos)
        id (bit-shift-right st 4)]
    (or (neg? st)
        (and (pos? id)
             (not (#{8 9} id))
             (not (fragile-id? id))))))

(defn supported? [chunks template pos st]
  (let [id  (bit-shift-right (long st) 4)
        m   (bit-and (long st) 15)
        off (cond
              (torch-ids id)  (torch-support m [0 -1 0])
              (side-ids id)   (side-support m [0 0 1])
              (ground-ids id) [0 -1 0]
              :else nil)]
    (if off
      (solid-at? chunks template (mapv + pos off))
      true)))

(def rule
  {:name   :fragile
   :match? (fn [_chunks st _p] (fragile? st))
   :wake   (fn [_chunks tick _p _old _self?] (inc (long tick)))
   :due    (fn [chunks p]
             (let [st (chunk/chunks-get-block chunks gen/flat-chunk p)]
               (when-not (supported? chunks gen/flat-chunk p st)
                 [[p 0]])))})
