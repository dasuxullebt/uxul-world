;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defclass anchor (game-object)
  ((dungeon :initform nil
	    :initarg :dungeon
	    :accessor dungeon))
   (:documentation "This object ist just to make it easier to handle
   positions in the game, i.e. for bounding-rects for
   burning-marshmallows, etc."))

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

(defclass teleporter (game-object-with-animation)
  ((animation :initarg :animation
	      :accessor :animation
	      :initform (make-animation 0 |teleporter|))
   (width :initarg :width
	  :accessor width
	  :initform 128)
   (height :initarg :height
	   :accessor height
	   :initform 128)
   (next-room-function :initarg :next-room-function
		       :accessor next-room-function
		       :initform (lambda () *current-room*))))

(defmethod invoke ((obj teleporter))
  (let
      ((player (car (get-objects *current-room* 'player))))
    (cond
      ((rectangles-overlap (x obj) (y obj)
			   (+ (x obj) (width obj))
			   (+ (y obj) (height obj))
			   (x player) (y player)
			   (+ (x player) (width player))
			   (+ (y player) (height player)))
       (if (key-pressed-up player)
	   ;; change the room
	   (setf *current-room* (funcall (next-room-function obj)))))
      (T (setf (active obj) nil)))))
