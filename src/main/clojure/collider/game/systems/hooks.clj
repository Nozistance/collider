(ns collider.game.systems.hooks
  "The systems through which plugins hear the events of players."
  (:require [collider.game.apply :as apply]
            [collider.game.deltas :as deltas]))

(set! *warn-on-reflection* true)

(defn on-event
  "Returns a system that gives each event of this tick whose tag
  handlers holds to the function of that tag, as (f level event)
  that returns deltas."
  [handlers]
  (let [tags (set (keys handlers))
        heard? #(contains? tags (nth % 0))
        one (fn [w ev] ((get handlers (nth ev 0)) w ev))]
    (with-meta
      (fn [lv d]
        (let [evs (filterv heard? (:input d))]
          (deltas/of-vec (apply/fold-events lv evs one))))
      {:wake {:events tags}})))
