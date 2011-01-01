;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defclass collision ()
  ((pos :initarg :pos
	:accessor pos
	:type xy-struct
	:documentation "The position where the moving rectangle is at
	the point of collision")
   (direction :initarg :direction
	      :accessor direction
	      :documentation "On which side of the MOVING rectangle
	      does the collision occur?")
   (collision-time :initarg :collision-time
		   :accessor collision-time
		   :type rational
		   :documentation "The quotient of the length of the
		   real movement and the length of the desired
		   movement.")
   (desired-movement :initarg :desired-movement
		     :accessor desired-movement
		     :type xy-struct
		     :documentation "The full movement that was given
   to move-about and could not be fulfilled.")))

(defmethod has-horizontal-direction ((obj collision))
  "test, whether this has horizontal direction"
  (or (eq (direction obj) :left)
      (eq (direction obj) :right)))

(defmethod has-vertical-direction ((obj collision))
  "test, whether this has vertical direction"
  (or (eq (direction obj) :up)
      (eq (direction obj) :down)))

(defmethod colliding ((object null))
  nil)