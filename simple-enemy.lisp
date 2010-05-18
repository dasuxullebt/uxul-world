;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defclass simple-enemy (moving-enemy)
  ((animation :initarg :animation
	      :initform
	      (make-animation 3 |nasobem| |nasobem2|)
	      :accessor animation)
   (animation-translation :accessor animation-translation
			  :initarg :animation-translation
			  :initform (make-xy -100 -50))
   (flat-animation :accessor flat-animation
		   :initform (make-animation 0 |nasobem3|))
   (dont-ignore :accessor dont-ignore :initform t)
   (activated :accessor activated :initform nil)
   (width :initarg :width :initform 64 :accessor width)
   (active :initarg :active :initform t :accessor active)
   (height :initarg :height :initform 64 :accessor height)
   (direction :initarg :direction :initform :left :accessor direction)))

(defmethod invoke ((obj simple-enemy))
  "Move the object down-left if direction is :left"
  (cond
    ((activated obj) (move-about obj (make-xy
				      (if (eql (direction obj) :left)
					  -10
					  10) 10)))
    (T
     (dolist (player (get-objects *current-room* 'player))
       (if (and
	    (< (abs (- (x player) (x obj))) (+ +screen-width+ 300))
	    (< (abs (- (y player) (y obj))) (+ +screen-height+ 300)))
	   (setf (activated obj) T))))))

(defun simple-enemy-and-player (player enemy)
  (decf (power player)))

(defmethod player-hits-enemy ((player player) (enemy simple-enemy) &rest args)
  (cond
    ((eql (direction (car args)) :DOWN)
     (setf (animation enemy) (flat-animation enemy))
     (setf (active enemy) nil)
     (setf (colliding enemy) nil)
     (setf (listen-to player) (remove enemy (listen-to player))))
    (T
     (simple-enemy-and-player player enemy))))

(defmethod enemy-hits-player ((enemy simple-enemy) (player player) &rest args)
    (declare (ignore args))
    (simple-enemy-and-player player enemy))