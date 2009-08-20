;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defclass leaf (bottom)
  ((animation :initarg :animation
	      :accessor animation
	      :initform (make-animation 0 |leaf|))
   (width :initarg :width
	  :accessor :width
	  :initform 128)
   (height :initarg :height
	   :accessor :height
	   :initform 3)
   (animation-translation :initarg :animation-translation
			  :accessor animation-translation
			  :initform (make-xy -7 -30))
   ))

(defclass tulip (standing-item)
  ((animation :initarg :animation
	      :accessor animation
	      :initform (make-animation 10 |tulip| |tulip2| |tulip| |tulip3|))
   (width :initarg :width
	  :accessor :width
	  :initform 128)
   (height :initarg :height
	   :accessor :height
	   :initform 128)))

(defclass key (standing-item)
  ((animation :initarg :animation
	      :accessor animation
	      :initform (make-animation 0 |key|))
   (width :initarg :width
	  :accessor width
	  :initform 128)
   (height :initarg :height
	   :accessor height
	   :initform 128)
   (dungeon :initarg :dungeon
	    :accessor dungeon
	    :initform nil
	    :documentation "To provide information in which rooms this key can be used.")))

(defclass door (stone)
  ((animation :initarg :animation
	      :accessor animation
	      :initform (make-animation 0 |door|))
   (width :initarg :width
	  :accessor width
	  :initform 128)
   (height :initarg :height
	   :accessor height
	   :initform 128)
   (dungeon :initarg :dungeon
	    :accessor dungeon
	    :initform nil
	    :documentation "To provide information in which room this door is.")))