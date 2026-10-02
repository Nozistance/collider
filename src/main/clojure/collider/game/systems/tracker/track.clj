(ns collider.game.systems.tracker.track
  "Entity tracker records of what a player last knew of a body.")

(defrecord Track
  [pos yaw pitch head on-ground mdata equip vel-sent since-tp slots
   carried t0])

(defrecord Frame
  [x y z dx dy dz yaw pitch head ground since due? vel mdata mdiff
   equip moved? turned? rel? head-turned? meta-changed?
   equip-changed? vel-changed? equip-diff slot-diff carried-changed?
   first?])
