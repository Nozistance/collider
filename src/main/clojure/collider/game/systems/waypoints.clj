(ns collider.game.systems.waypoints
  "The locator bar. Each player who transmits is a point on the bar
  of each other player of its level, as a block, a chunk or a bearing
  by how far it is. A link changes only when one of the two moves,
  or when the one who transmits starts or stops."
  (:require [collider.game.attribute :as attribute]
            [collider.game.deltas :as deltas]
            [collider.game.level :as level]
            [collider.game.mode :as game-mode]
            [collider.game.out :as out]
            [collider.game.systems.chunks :as chunks]
            [collider.num :as num]
            [collider.vec :as v]
            [collider.world.chunk :as chunk]))

(set! *warn-on-reflection* true)

(def ^:private ^:const really-far 332.0)

(def ^:private turn (num/f32 0.008726646))

(defn- block-of [e]
  (let [p (:pos e)]
    [(long (Math/floor (v/x p))) (long (Math/floor (v/y p)))
     (long (Math/floor (v/z p)))]))

(defn- chunk-of [e] (chunk/id->pos (chunk/pos-chunk (:pos e))))

(defn- distance
  "Returns the distance of a to b, counted in floats."
  ^double [a b]
  (let [p (:pos a) q (:pos b)
        x (num/f32 (- (v/x p) (v/x q)))
        y (num/f32 (- (v/y p) (v/y q)))
        z (num/f32 (- (v/z p) (v/z q)))
        sq (num/f32 (+ (num/f32 (+ (num/f32 (* x x)) (num/f32 (* y y))))
                       (num/f32 (* z z))))]
    (num/f32 (Math/sqrt sq))))

(defn- bearing
  "Returns the angle at which r sees s, a float."
  ^double [s r]
  (let [p (:pos r) q (:pos s)]
    (num/f32 (num/atan2 (- (v/x p) (v/x q)) (- (- (v/z p) (v/z q)))))))

(defn- transmit ^double [s]
  (if (:crouched? (:waypoint s))
    0.0
    (attribute/value s (:effects s) :waypoint-transmit-range)))

(defn- ignores? [s r]
  (and (not (game-mode/spectator? r))
       (or (game-mode/spectator? s)
           (>= (distance s r)
               (min (transmit s)
                    (attribute/value r (:effects r)
                                     :waypoint-receive-range))))))

(defn- in-view?
  "Returns true when chunk [cx cz] is in the view distance of r. One
  ring past the axes counts as distance zero."
  [world r [cx cz]]
  (let [[x z] (if-let [cp (:chunk-pos r)] (chunk/id->pos cp) (chunk-of r))
        v (long (or (:chunk-view r) (chunks/player-radius world r)))
        dx (max 0 (dec (abs (- (long cx) (long x)))))
        dz (max 0 (dec (abs (- (long cz) (long z)))))]
    (< (+ (* dx dx) (* dz dz)) (* v v))))

(defn- first-tick? [world e] (= (:born e) (:tick world)))

(defn- link
  "Returns the link r gets to s, or nil when r gets none."
  [world s r]
  (when-not (or (first-tick? world s) (ignores? s r))
    (cond
      (> (distance s r) really-far) {:kind :azimuth :at (bearing s r)}
      (not (in-view? world r (chunk-of s)))
      {:kind :chunk :at (chunk-of s)}
      :else {:kind :block :at (block-of s)})))

(defn- apart ^long [a b]
  (reduce + (map #(abs (- (long %1) (long %2))) a b)))

(defn- far-apart ^long [a b]
  (reduce max (map #(abs (- (long %1) (long %2))) a b)))

(defn- broken? [world s r {:keys [kind at]}]
  (case kind
    :block (or (> (apart at (block-of s)) 1) (ignores? s r))
    :chunk (or (> (far-apart at (chunk-of s)) 1) (ignores? s r)
               (in-view? world r at))
    :azimuth (or (ignores? s r) (in-view? world r (chunk-of s))
                 (not (> (distance s r) really-far)))))

(defn- now-at [s r kind]
  (case kind
    :block (block-of s)
    :chunk (chunk-of s)
    :azimuth (bearing s r)))

(defn- moved? [kind at at']
  (if (= :azimuth kind)
    (> (abs (num/f32 (- (double at') (double at)))) turn)
    (not= at at')))

(defn- sent [st rid op uuid kind at]
  (update st :out conj (out/to rid (out/waypoint op uuid kind at))))

(defn- unlinked [st rid sid]
  (if-let [c (get-in st [:links rid sid])]
    (-> (update-in st [:links rid] dissoc sid)
        (sent rid :untrack (:uuid c) nil nil))
    st))

(defn- linked
  "Returns st with r linked to s afresh, as createConnection does."
  [st world [sid s] [rid r]]
  (cond
    (= sid rid) st
    (not (:on? st)) st
    :else
    (if-let [c (link world s r)]
      (-> (assoc-in st [:links rid sid] (assoc c :uuid (:uuid s)))
          (sent rid :track (:uuid s) (:kind c) (:at c)))
      (unlinked st rid sid))))

(defn- touched
  "Returns st with the link of r to s brought up to date, as
  updateConnection does, or made when there is none."
  [st world [sid s :as se] [rid r :as re]]
  (let [c (get-in st [:links rid sid])]
    (cond
      (or (= sid rid) (not (:on? st))) st
      (or (nil? c) (broken? world s r c)) (linked st world se re)
      :else
      (let [{:keys [kind at]} c at' (now-at s r kind)]
        (if (moved? kind at at')
          (-> (assoc-in st [:links rid sid :at] at')
              (sent rid :update (:uuid c) kind at'))
          st)))))

(defn- untracked [st sid]
  (reduce #(unlinked %1 %2 sid) st (keys (:links st))))

(defn- in-set [st ps]
  (filter #(contains? (:in st) (key %)) ps))

(defn- gone [st es]
  (reduce-kv (fn [st rid ls]
               (reduce #(if (contains? es %2) %1 (unlinked %1 rid %2))
                       st (keys ls)))
             st (:links st)))

(defn- transmits? [s] (> (transmit s) 0.0))

(defn- cut [st rid]
  (reduce #(unlinked %1 rid %2) st (keys (get-in st [:links rid]))))

(defn- born [st world [pid p :as pe] ps]
  (if (first-tick? world p)
    (let [st (-> (cut st pid)
                 (untracked pid)
                 (assoc-in [:links pid] {}))
          st (reduce #(touched %1 world %2 pe) st (in-set st ps))]
      (cond-> st (transmits? p) (update :in conj pid)))
    st))

(defn- stepped [st world [sid s :as se] ps]
  (if (and (contains? (:in st) sid) (not (first-tick? world s))
           (not= (block-of s) (:block (:waypoint s))))
    (reduce #(touched %1 world se %2) st ps)
    st))

(defn- heard [st world [rid r :as re] ps moved]
  (if (and (not (first-tick? world r))
           (or (contains? moved rid) (nil? (:waypoints r))))
    (reduce #(touched %1 world %2 re) st (in-set st ps))
    st))

(defn- switched [st world [sid s :as se] ps]
  (let [was (contains? (:in st) sid) now (transmits? s)]
    (cond
      (and was (not now)) (-> (untracked st sid) (update :in disj sid))
      (and now (not was))
      (reduce #(linked %1 world se %2) (update st :in conj sid) ps)
      :else st)))

(defn- broke-all [st]
  (reduce cut st (keys (:links st))))

(defn- movers [world]
  (into #{} (keep (fn [[tag eid]] (when (= :move tag) eid)))
        (get-in world [:input :heeded])))

(defn- marks [st [eid e]]
  (let [mark {:in? (contains? (:in st) eid) :block (block-of e)
              :crouched? (= :crouching (:pose e))}
        ls (when (:on? st) (get (:links st) eid {}))]
    (when (or (not= mark (:waypoint e)) (not= ls (:waypoints e)))
      [:merge-entity eid {:waypoint mark :waypoints ls}])))

(defn waypoints
  "Returns the deltas that keep the locator bar of every player."
  {:wake {:types #{:player}}}
  [world _d]
  (let [ps (vec (level/player-entries world))
        on? (get-in world [:rules :locator-bar] true)
        st {:on? on? :out []
            :in (into #{} (keep (fn [[eid e]]
                                  (when (:in? (:waypoint e)) eid)))
                      ps)
            :links (into {} (map (fn [[eid e]]
                                   [eid (:waypoints e {})]))
                         ps)}
        st (gone st (set (map key ps)))
        st (reduce #(born %1 world %2 ps) st ps)
        st (reduce #(stepped %1 world %2 ps) st ps)
        moved (movers world)
        st (reduce #(heard %1 world %2 ps moved) st ps)
        st (reduce #(switched %1 world %2 ps) st ps)
        st (if on? st (broke-all st))]
    (deltas/of-vec (into (:out st) (keep #(marks st %)) ps))))
