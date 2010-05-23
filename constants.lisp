;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defconstant +screen-width+ 1024)
(defconstant +screen-height+ 768)

(defconstant +class-indices+ '(t uxul-world::animation
  uxul-world::collision uxul-world::game-object uxul-world::player
  uxul-world::room uxul-world::stone uxul-world::xy-coordinates
  uxul-world::bottom uxul-world::moving-enemy
  uxul-world::standing-enemy uxul-world::moving-item
  uxul-world::standing-item uxul-world::game-object-with-animation
  uxul-world::teleporter))