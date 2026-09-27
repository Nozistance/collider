(ns collider.tables.progress
  "Events that tell how the work goes.")

(set! *warn-on-reflection* true)

(def ^:dynamic *progress* (fn [_] nil))

(defn progress! [m]
  (*progress* m))

(defn measured
  "Runs f and sends the :end event of step with its time."
  [step f]
  (let [t (System/nanoTime)
        v (f)]
    (progress! (assoc v :event :end :step step
                      :took (- (System/nanoTime) t)))
    v))

(defn timed
  "Runs f between the :begin and the :end event of step."
  [step f]
  (progress! {:event :begin :step step})
  (measured step f))
