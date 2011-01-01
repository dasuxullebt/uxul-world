;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defclass stone (game-object-with-animation)
  ((animation :initarg :animation
	      :initform (make-animation 0 |gray_stone|)
	      :accessor animation)
   (width :initarg :width
	  :accessor width
	  :initform 128)
   (height :initarg :height
	   :accessor height
	   :initform 128)
   (active :initarg :active
	   :accessor active
	   :initform NIL)
   (redraw :accessor redraw
	   :initform t))
  (:documentation
"Defines an object that cannot be passed by enemies or the player or
  items per default."))

(defclass bottom (stone)
  ((animation :initarg :animation
	      :initform (make-animation 0 |block|)
	      :accessor animation)
   (width :initarg :width
	  :accessor width
	  :initform 64)
   (height :initarg :height
	   :accessor height
	   :initform 64)
   (active :initarg :active
	   :accessor active
	   :initform NIL)
   (redraw :accessor redraw
	   :initform t))
  (:documentation
"Defines an object that cannot be passed from the top side, but can be
  passed from all other sides by the player, enemies and items per
  default."))

(defclass moving-enemy (game-object-with-animation)
  ((animation :initarg :animation
	      :initform (make-animation 0 |block|)
	      :accessor animation)
   (width :initarg :width
	  :accessor width
	  :initform 64)
   (height :initarg :height
	   :accessor height
	   :initform 64)
   (active :initarg :active
	   :accessor active
	   :initform t)
   (visible :initarg :visible
	    :accessor visible
	    :initform t)	   
   (redraw :accessor redraw
	   :initform t)
   )
  (:documentation
"The default class for moving enemies. This class cannot pass through
stones and bottoms, and listens to the player."))

(defclass standing-enemy (stone) () (:documentation
"The default class for standing enemies."))

(defclass standing-item (game-object-with-animation) () (:documentation
"The default class for standing items."))

(defclass moving-item () () (:documentation
"The default class for moving items."))