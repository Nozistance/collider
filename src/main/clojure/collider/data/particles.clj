(ns collider.data.particles
  "The shapes particle options take, by particle type.")

(set! *warn-on-reflection* true)

(def ^:private option-kinds
  {:block :state :block-marker :state :falling-dust :state
   :dust-pillar :state :block-crumble :state
   :entity-effect :color :tinted-leaves :color :flash :color
   :trail :trail :dragon-breath :power :dust :dust
   :dust-color-transition :transition :effect :spell
   :instant-effect :spell :sculk-charge :roll :item :item
   :vibration :vibration :shriek :delay :geyser :geyser
   :geyser-plume :geyser :geyser-base :geyser-base
   :geyser-poof :geyser-base})

(defn kind
  "Returns the shape of the options of particle type k, :none when it
  has none."
  [k]
  (get option-kinds k :none))
