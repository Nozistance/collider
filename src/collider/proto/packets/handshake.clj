(ns collider.proto.packets.handshake
  (:require [collider.proto.spec :refer [defpacket]]))

(set! *warn-on-reflection* true)

(defpacket ::handshake {:state :handshaking :dir :c->s :id 0x00}
  [:protocol   :varint]
  [:address    :string]
  [:port       :ushort]
  [:next-state :varint])
