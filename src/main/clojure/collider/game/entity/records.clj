(ns collider.game.entity.records
  "Entity record types.")

(defrecord Mob [pos vel on-ground yaw pitch head-yaw walked wet?
                jump-cd task follow look no-action say-tick health
                hurt-resist last-damage death-time health-sent
                panic-until baby-until love-until breed-ready-at
                tempt-cooldown-until type color sheared? track
                nav move body jump float? support no-blocks? in-lava?
                follow-at effects arrived stick-cooldown-until
                egg-at hop fall])

(defrecord Player [type name uuid pos yaw pitch on-ground client-vel
                   tp-target chunk-pos sent-chunks
                   tracking track health hurt-resist last-damage
                   death-time health-sent inventory held-slot
                   using-item? sneaking? sprinting? skin-parts
                   view-distance chunk-view ping
                   keepalive-at keepalive-pending?])

(defrecord Item [type pos vel yaw pitch on-ground stack age
                 pickup-delay needs-sync? track])

(defrecord Orb [type pos vel yaw pitch on-ground value count age
                health follow track])

(defrecord Tnt [type pos vel yaw pitch on-ground origin fuse owner
                arrived track])

(defrecord FallingBlock
  [type pos vel yaw pitch on-ground block start time track fall hurt
   hurt-max])

(defrecord Projectile [type pos vel yaw pitch on-ground stack owner
                       age left-owner? track])

(defrecord Cloud [type pos vel yaw pitch on-ground radius color
                  waiting? age duration wait-time radius-per-tick
                  radius-on-use victims track])

(defrecord Hanging [type pos vel yaw pitch on-ground block-pos facing
                    variant stack rotation check-at track])
