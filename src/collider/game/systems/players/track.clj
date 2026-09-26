(ns collider.game.systems.players.track
  "The records of the entity tracker: what a player was last told
  of a body, and the frame of one update.")

(defrecord Track
  [pos yaw pitch head on-ground mdata equip vel-sent since-tp slots
   carried seen t0])

(defrecord Frame
  [x y z dx dy dz yaw pitch head ground since due? vel mdata mdiff
   equip moved? turned? rel? head-turned? meta-changed?
   equip-changed? vel-changed? equip-diff slot-diff carried-changed?
   first?])
