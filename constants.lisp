;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defmacro mydefconst (x y)
  `(eval-when (:compile-toplevel :load-toplevel)
     (when (or (not (boundp ',x))
	       (not (equal ,y
			   (symbol-value ',x))))
       (defconstant ,x ,y))))


(mydefconst +screen-width+ 1024)
(mydefconst +screen-height+ 768)

(mydefconst +class-indices+ '(t uxul-world::animation
  uxul-world::collision uxul-world::game-object uxul-world::player
  uxul-world::room uxul-world::stone uxul-world::xy-coordinates
  uxul-world::bottom uxul-world::moving-enemy
  uxul-world::standing-enemy uxul-world::moving-item
  uxul-world::standing-item uxul-world::game-object-with-animation
  uxul-world::teleporter))