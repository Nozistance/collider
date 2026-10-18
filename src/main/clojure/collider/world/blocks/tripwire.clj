(ns collider.world.blocks.tripwire
  "Tripwire hooks and the string between them."
  (:require [collider.num :as num]
            [collider.random :as random]
            [collider.world.block :as block]
            [collider.world.blocks.support :as support]
            [collider.world.chunk :as chunk]
            [collider.world.direction :as dir]
            [collider.world.update :as update]))

(set! *warn-on-reflection* true)

(def ^:const recheck-ticks
  "The ticks a hook waits to look at its line again."
  10)

(def ^:const span
  "How far a hook looks along its line, exclusive."
  42)

(defn hook?
  [^long st]
  (= :trip-wire-hook (block/type-of st)))

(defn wire?
  [^long st]
  (= :tripwire (block/type-of st)))

(defn- on? [^long st k] (= :true (k (block/props-of st))))

(defn- partner [^long st f i]
  (if (= (block/facing-of st) (dir/opposite f)) i 0))

(defn- scanned
  "Returns [far wires attached powered hit] of the line a hook at p
  facing f sees, with wire in cell source. far is the cell of the
  hook facing back or 0, hit whether the line holds source."
  [chunks p f source wire]
  (loop [i 1 wires {} attached true powered false hit false]
    (let [st (chunk/at chunks (dir/toward p f i))
          here? (= i source)
          ws (if here? wire st)
          armed (not (on? ws :disarmed))]
      (cond
        (= i span) [0 wires attached powered hit]
        (hook? st) [(partner st f i) wires attached powered hit]
        (not (or here? (wire? st)))
        (recur (inc i) wires false powered hit)
        :else (recur (inc i) (assoc wires i ws)
                     (and attached (or (not here?) armed))
                     (or powered (and armed (on? ws :powered)))
                     (or hit here?))))))

(defn- sound [kind pitch event]
  [[:sound kind (num/f32 0.4) (num/f32 pitch)] [:game-event event]])

(defn- detach-pitch
  "Returns the pitch of the detach sound for the random float r, in
  float steps."
  ^double [^double r]
  (let [f num/f32]
    (f (/ (f 1.2) (f (+ (f (* r (f 0.2))) (f 0.9)))))))

(defn- emitted
  "Returns the fx of a hook at p turning from was to now, each
  [attached powered]."
  [p tick [a pw] [was-a was-pw]]
  (cond
    (and pw (not was-pw)) (sound :block.tripwire.click-on 0.6
                                 :block-activate)
    (and (not pw) was-pw) (sound :block.tripwire.click-off 0.5
                                 :block-deactivate)
    (and a (not was-a)) (sound :block.tripwire.attach 0.7
                               :block-attach)
    (and (not a) was-a)
    (sound :block.tripwire.detach
           (detach-pitch (num/f32 (random/of-key tick p :detach)))
           :block-detach)))

(defn- heard [p fx]
  (fn [cs] (when fx [[p (chunk/at cs p) fx]])))

(defn- told [p ^long st]
  (let [back (dir/opposite (block/facing-of st))]
    [[:notify p nil st] [:notify (dir/toward p back) nil st]]))

(defn- reattached [q ws attached]
  (fn [cs]
    (let [cur (chunk/at cs q)]
      (when (or (wire? cur) (hook? cur))
        [[q (block/with ws :attached (block/flag attached)) nil
          update/all]]))))

(declare removed)

(defn- was [^long st] [(on? st :attached) (on? st :powered)])

(defn- own
  "Returns the changes of the hook at p after its partner took the
  new state, which are its sound, its state and the string it holds."
  [p f st new now {:keys [gone? told? tick]} far wires]
  (let [own (block/with new :facing f)]
    (cond-> [(heard p (emitted p tick now (was st)))]
      (not gone?) (conj [p own nil update/all])
      (and told? (not gone?)) (into (told p own))
      (not= ((was st) 0) (now 0))
      (into (for [i (range 1 far) :let [ws (wires i)] :when ws]
              (reattached (dir/toward p f i) ws (now 0)))))))

(defn- partnered
  "Returns the changes of the partner of the hook st at p, far cells
  along f, before tail, the changes of the hook itself."
  [p st f far new now tick tail]
  (let [q (dir/toward p f far)
        far-st (block/with new :facing (dir/opposite f))
        fx (emitted q tick now (was st))]
    (-> [[q far-st nil update/all]]
        (into (told q far-st))
        (conj (fn [cs]
                (if (hook? (chunk/at cs p))
                  (into [(heard q fx)] tail)
                  (removed p new tick)))))))

(defn- calculated
  "Returns the changes of the hook st at p that looks along its line.
  opts are :gone? while it goes, :told? to tell its cells, :source
  and :wire for the string that asks and its state, and :tick."
  [chunks p ^long st {:keys [gone? source wire tick] :as opts}]
  (let [f (block/facing-of st)
        [far wires a pw hit] (scanned chunks p f (or source -1) wire)
        far (long far)
        a (and a (not gone?) (> far 1))
        now [a (and pw a)]
        new (block/with (block/state :tripwire-hook {})
                        :attached (block/flag a)
                        :powered (block/flag (now 1)))
        tail (own p f st new now opts far wires)]
    (cond-> (if hit [[:tick p (+ (long tick) recheck-ticks) 0
                      :tripwire-hook]] [])
      (zero? far) (into tail)
      (pos? far) (into (partnered p st f far new now tick tail)))))

(defn removed
  "Returns the changes of hook st going from p."
  [p ^long st tick]
  (let [pw (on? st :powered)]
    (cond-> []
      (or pw (on? st :attached))
      (conj (fn [cs] (calculated cs p st {:gone? true :tick tick})))
      pw (into (told p st)))))

(defn placed-by
  "Returns the step of a hook placed at p by a hand or an item."
  [p tick]
  (fn [cs]
    (let [st (chunk/at cs p)]
      (when (hook? st) (calculated cs p st {:tick tick})))))

(defn- seek [cs p d ws tick]
  (loop [i 1]
    (let [st (chunk/at cs (dir/toward p d i))]
      (cond
        (= i span) nil
        (hook? st) (when (= (block/facing-of st) (dir/opposite d))
                     (calculated cs (dir/toward p d i) st
                                 {:told? true :source i :wire ws
                                  :tick tick}))
        (wire? st) (recur (inc i))))))

(defn sourced
  "Returns the steps of string ws at p that wake the hooks of its
  lines, the south one first and the west one next."
  [p ^long ws tick]
  (mapv (fn [d] (fn [cs] (seek cs p d ws tick))) [:south :west]))

(defn powered?
  [^long st]
  (on? st :powered))

(defn checked
  "Returns the ops of string st at p that a body holds down when
  pressed?, at tick t."
  [p ^long st pressed? t]
  (let [was (on? st :powered)
        st' (block/with st :powered (block/flag pressed?))]
    (cond-> []
      (not= was pressed?) (into (cons [p st' nil update/all]
                                      (sourced p st' t)))
      pressed? (conj [:tick p (+ (long t) recheck-ticks) 0 :tripwire])
      (and was (not pressed?)) (conj [:tick p t 0 :tripwire]))))

(defn- bounds [p ^long st]
  (let [[b & more] (block/outline-boxes st)]
    (when more
      (throw (ex-info "string of more than one box" {:state st})))
    (mapv (fn [c x] (+ (long c) (/ (double x) 16.0))) (into p p) b)))

(defn- due [chunks p {:keys [tick bodies]}]
  (let [st (chunk/at chunks p)]
    (when (and (wire? st) (on? st :powered))
      (checked p st (pos? (long (bodies (bounds p st) false)))
               tick))))

(defn- placed [chunks p {:keys [old tick]}]
  (when-not (wire? (long old))
    (sourced p (chunk/at chunks p) tick)))

(def wire-rule
  "The block rule of string, which wakes its hooks once placed and
  looks for the bodies on it on its tick."
  {:name    :tripwire
   :match?  (fn [_chunks st _p] (wire? st))
   :pass    :place
   :reach   0
   :wake    (constantly :neighbor)
   :reshape placed
   :due     due})

(def hook-rule
  "The block rule of hooks. They are held as attached blocks are and
  look along their line on their tick."
  (assoc support/rule
         :name :tripwire-hook
         :match? (fn [_chunks st _p] (hook? st))
         :reach 0
         :due (fn [_chunks p {:keys [tick]}]
                [(fn [cs]
                   (calculated cs p (chunk/at cs p)
                               {:told? true :tick tick}))])))
