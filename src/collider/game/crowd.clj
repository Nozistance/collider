(ns collider.game.crowd

  (:require [clojure.data.int-map :as im]
            [collider.game.mobs :as mobs]
            [collider.game.state :as state]
            [collider.rnd :as rnd]
            [collider.vec :as v]))

(set! *warn-on-reflection* true)

(defn- pushable-half ^double [e]
  (case (:type e)
    :player 0.3
    (double (get-in mobs/types [(:type e) :half] 0.0))))

(defn- pushable-height ^double [e]
  (case (:type e)
    :player 1.8
    (double (get-in mobs/types [(:type e) :height] 1.0))))

(deftype PushCell [^longs eids ^doubles xs ^doubles ys ^doubles zs
                   ^doubles halfs ^doubles heights])

(defn- push-cell-key ^long [pos]
  (bit-or (bit-shift-left (bit-and (bit-shift-right (long (Math/floor (v/x pos))) 2) 0xFFFFFFFF) 32)
          (bit-and (bit-shift-right (long (Math/floor (v/z pos))) 2) 0xFFFFFFFF)))

(defn- cell-key ^long [^long cx ^long cz]
  (bit-or (bit-shift-left (bit-and cx 0xFFFFFFFF) 32) (bit-and cz 0xFFFFFFFF)))

(def ^:private ^:const push-cap 16)
(defn push-index [world active]
  (let [groups (persistent!
                (reduce (fn [m [eid e]]
                          (if (and (or (= :player (:type e)) (mobs/mob-type? (:type e)))
                                   (state/active-at? active (:pos e)))
                            (let [k (push-cell-key (:pos e))]
                              (assoc! m k (conj (get m k []) [eid e])))
                            m))
                        (transient (im/int-map))
                        (:entities world)))]
    (persistent!
     (reduce-kv (fn [m k entries]
                  (let [entries (sort-by first entries)
                        n (count entries)
                        eids (long-array n) xs (double-array n) ys (double-array n)
                        zs (double-array n) halfs (double-array n) heights (double-array n)]
                    (loop [i 0 es (seq entries)]
                      (when es
                        (let [[eid e] (first es)
                              p (:pos e) x (v/x p) y (v/y p) z (v/z p)]
                          (aset eids i (long eid))
                          (aset xs i (double x)) (aset ys i (double y)) (aset zs i (double z))
                          (aset halfs i (pushable-half e)) (aset heights i (pushable-height e))
                          (recur (inc i) (next es)))))
                    (assoc! m k (PushCell. eids xs ys zs halfs heights))))
                (transient (im/int-map))
                groups))))

(deftype Window [^objects cells ^longs sizes ^long self-i ^long n])
(defn- push-window ^Window [index ^long cx ^long cz ^long eid]
  (let [cells (object-array 9)
        sizes (long-array 9)
        self-i (long (if-let [^PushCell own (get index (cell-key cx cz))]
                       (let [^longs ids (.eids own)]
                         (loop [i 0]
                           (cond (= i (alength ids)) -1
                                 (= (aget ids i) eid) i
                                 :else (recur (inc i)))))
                       -1))
        n (loop [c 0 n 0]
            (if (= c 9)
              n
              (let [^PushCell cell (get index (cell-key (+ cx (dec (quot c 3))) (+ cz (dec (rem c 3)))))
                    sz (if cell
                         (- (alength ^longs (.eids cell)) (if (and (= c 4) (>= self-i 0)) 1 0))
                         0)]
                (aset cells c cell) (aset sizes c sz)
                (recur (inc c) (+ n sz)))))]
    (Window. cells sizes self-i n)))

(defn- push-into! [^doubles acc ^Window w ^long i ^doubles me]
  (let [^longs sizes (.sizes w)
        ^objects cells (.cells w)]
    (loop [c 0 i i]
      (let [sz (aget sizes c)]
        (if (< i sz)
          (let [^PushCell cell (aget cells c)
                j  (if (and (= c 4) (>= (.self-i w) 0) (>= i (.self-i w))) (inc i) i)
                x  (aget me 0) y (aget me 1) z (aget me 2)
                ox (aget ^doubles (.xs cell) j)
                oy (aget ^doubles (.ys cell) j)
                oz (aget ^doubles (.zs cell) j)
                reach (+ (aget me 3) (aget ^doubles (.halfs cell) j) 0.2)]
            (when (and (< (Math/abs (- ox x)) reach)
                       (< (Math/abs (- oz z)) reach)
                       (< oy (+ y (aget me 4)))
                       (> (+ oy (aget ^doubles (.heights cell) j)) y))
              (let [dx (- ox x) dz (- oz z)
                    m  (max (Math/abs dx) (Math/abs dz))]
                (when (>= m 0.01)
                  (let [s  (Math/sqrt m)
                        d3 (min 1.0 (/ 1.0 s))]
                    (aset acc 0 (+ (aget acc 0) (- (* (/ dx s) d3 0.1))))
                    (aset acc 1 (+ (aget acc 1) (- (* (/ dz s) d3 0.1)))))))))
          (recur (inc c) (- i sz)))))))

(defn- push-span! [^doubles acc ^Window w from to ^doubles me]
  (loop [i (long from)]
    (when (< i (long to)) (push-into! acc w i me) (recur (inc i)))))

(defn crowd-push [index eid e t half height]
  (let [p (:pos e) x (v/x p) y (v/y p) z (v/z p)
        x (double x) z (double z)
        ^Window w (push-window index (bit-shift-right (long (Math/floor x)) 2)
                               (bit-shift-right (long (Math/floor z)) 2) (long eid))
        me  (double-array [x (double y) z (double half) (double height)])
        acc (double-array 2)
        n   (.n w)]
    (if (<= n push-cap)
      (push-span! acc w 0 n me)
      (let [off (mod (rnd/mix64 (unchecked-add (rnd/mix64 t) (long eid))) n)
            end (+ off push-cap)]
        (if (<= end n)
          (push-span! acc w off end me)
          (do (push-span! acc w off n me)
              (push-span! acc w 0 (- end n) me)))))
    [(aget acc 0) (aget acc 1)]))
