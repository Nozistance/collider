(ns collider.game.mob.goals
  "The goal selector of a breed. Its goals come highest priority first,
  and goals of one priority never displace each other. A goal without
  a running? function runs while the task of the mob has its kind. A
  start that returns [e deltas false] declines to start but keeps what
  it changed on the mob.")

(set! *warn-on-reflection* true)

(def ^:private flags (range 4))

(defn- kind-of [e] (:kind (:task e)))

(defn- running? [g e t kind]
  (if-let [f (:running? g)]
    (boolean (f e t))
    (identical? (:kind g) kind)))

(defn- bits ^long [gs e t]
  (let [kind (kind-of e)]
    (reduce-kv (fn [^long b i g]
                 (if (running? g e t kind) (bit-set b i) b))
               0 gs)))

(defn- stopped [g e t]
  (if-let [f (:stop g)] (f e t) (assoc e :task nil)))

(defn- locks
  "Returns the priority that holds each flag among the goals running in
  b, -1 for a free flag. A later goal overwrites an earlier one."
  [gs ^long b]
  (reduce-kv (fn [held i g]
               (if (bit-test b i)
                 (reduce #(if (bit-test (:mask g) %2) (assoc %1 %2 (:prio g)) %1)
                         held flags)
                 held))
             [-1 -1 -1 -1] gs))

(defn- free? [locked g]
  (not-any? #(and (bit-test (:mask g) %)
                  (<= 0 (long (locked %)) (long (:prio g))))
            flags))

(defn- held? [locked g]
  (some #(and (bit-test (:mask g) %) (<= 0 (long (locked %)))) flags))

(defn- displaced [gs e t mask]
  (reduce (fn [e g]
            (if (and (pos? (bit-and (long (:mask g)) (long mask)))
                     (running? g e t (kind-of e)))
              (stopped g e t)
              e))
          e gs))

(defn- cleaned [gs w e t ts]
  (first (reduce (fn [[e kind] g]
                   (if (and (running? g e t kind)
                            (not ((:continue? g) w e t ts)))
                     (let [e (stopped g e t)] [e (kind-of e)])
                     [e kind]))
                 [e (kind-of e)] gs)))

(defn- declined? [r] (and (> (count r) 2) (not (nth r 2))))

(defn- started [gs w eid e t ts locked g]
  (let [r ((:start g) w eid e t ts)]
    (if (or (nil? r) (declined? r) (not (held? locked g)))
      r
      ((:start g) w eid (displaced gs e t (:mask g)) t ts))))

(defn- selected [gs w eid e t ts]
  (loop [i 0 e e ds [] b (bits gs e t) locked (locks gs (bits gs e t))]
    (if (= i (count gs))
      [e ds]
      (let [g (nth gs i)
            r (when (and (not (bit-test b i)) (free? locked g))
                (started gs w eid e t ts locked g))]
        (cond
          (nil? r) (recur (inc i) e ds b locked)
          (declined? r) (recur (inc i) (nth r 0) ds b locked)
          :else (let [e (nth r 0) b (bits gs e t)]
                  (recur (inc i) e (into ds (nth r 1)) b (locks gs b))))))))

(defn- ticked [spec w eid e t ts ds all?]
  (reduce (fn [[e ds] g]
            (let [f (:tick g)]
              (if (and f (or all? (:every-tick? g))
                       (running? g e t (kind-of e)))
                (let [r (f spec w eid e t ts)]
                  [(nth r 0) (into ds (nth r 1))])
                [e ds])))
          [e ds] (:goals spec)))

(defn think
  "Returns [e deltas] after one tick of the goals of mob e with id eid
  in world w at tick t. On a full tick the selector stops the goals
  that may not go on, starts those that may and ticks all. On other
  ticks it ticks only the goals that want every tick."
  [spec w eid e t ts full?]
  (let [gs (:goals spec)]
    (if full?
      (let [[e ds] (selected gs w eid (cleaned gs w e t ts) t ts)]
        (ticked spec w eid e t ts ds true))
      (ticked spec w eid e t ts [] false))))
