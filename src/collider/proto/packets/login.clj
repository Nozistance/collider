(ns collider.proto.packets.login
  (:require [collider.proto.spec :refer [defpacket]]))

(set! *warn-on-reflection* true)

(defpacket ::login-start {:state :login :dir :c->s :id 0x00}
  [:name :string])

(defpacket ::success {:state :login :dir :s->c :id 0x02}
  [:uuid     :string]
  [:username :string])
