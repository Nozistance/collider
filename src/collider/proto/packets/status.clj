(ns collider.proto.packets.status
  (:require [collider.proto.spec :refer [defpacket]]))

(set! *warn-on-reflection* true)

(defpacket ::request {:state :status :dir :c->s :id 0x00})
(defpacket ::ping {:state :status :dir :c->s :id 0x01}
  [:payload :long])

(defpacket ::response {:state :status :dir :s->c :id 0x00}
  [:json :string])

(defpacket ::pong {:state :status :dir :s->c :id 0x01}
  [:payload :long])
