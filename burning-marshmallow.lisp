;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defclass burning-marshmallow (moving-enemy)
  ((dont-ignore :accessor dont-ignore :initform t)
   (width :initarg :width :initform 64 :accessor width)
   (height :initarg :height :initform 64 :accessor height)
   (active :initarg :active :initform t :accessor active)
   (redraw :initarg :redraw :initform t :accessor redraw)
   (activated :initarg :activated :initform nil :accessor activated)
   ;; FIXME
   (animation :initarg :animation :initform
	      (make-animation 2
			      |burning_marshmallow_lu1|
			      |burning_marshmallow_lu2|)
	      :accessor animation)

   (lu-animation :initform (make-animation 2
					   |burning_marshmallow_lu1|
					   |burning_marshmallow_lu2|))
   (ld-animation :initform (make-animation 2
					   |burning_marshmallow_ld1|
					   |burning_marshmallow_ld2|))
   (ru-animation :initform (make-animation 2
					   |burning_marshmallow_ru1|
					   |burning_marshmallow_ru2|))
   (rd-animation :initform (make-animation 2
					   |burning_marshmallow_rd1|
					   |burning_marshmallow_rd2|))
   (inner-rectangle :initarg :inner-rectangle
		    :accessor inner-rectangle
		    :initform nil
		    :documentation
"An additional rectangle which the burning-marshmallow wont leave. Form: '(x1 y1 x2 y2). If nil, no bounds."
   )
   (horizontal-speed :initarg :horizontal-speed
		     :accessor horizontal-speed
		     :initform 20)
   (vertical-speed :initarg :vertical-speed
		   :accessor vertical-speed
		   :initform 20)
   (horizontal-direction :initarg :horizontal-direction
			 :accessor horizontal-direction
			 :initform :left)
   (vertical-direction :initarg :vertical-direction
		       :accessor vertical-direction
		       :initform :up)))

(defmethod invoke ((obj burning-marshmallow))
  (cond
    ((activated obj)
     (when (inner-rectangle obj)
       (cond
	 ((eql (horizontal-direction obj) :right)
	  (when (< (caddr (inner-rectangle obj))
		   (+ (x obj) (horizontal-speed obj)))
	    (setf (horizontal-direction obj) :left)
	    (set-burning-marshmallow-animation obj)))
	 (T ;; (eql (horizontal-direction obj) :left)
	  (when (> (car (inner-rectangle obj))
		   (- (x obj) (horizontal-speed obj)))
	    (setf (horizontal-direction obj) :right)
	    (set-burning-marshmallow-animation obj))))
       (cond
	 ((eql (vertical-direction obj) :down)
	  (when (< (cadddr (inner-rectangle obj))
		   (+ (y obj) (vertical-speed obj)))
	    (setf (vertical-direction obj) :up)
	    (set-burning-marshmallow-animation obj)))
	 (T ;; (eql (vertical-direction obj) :up)
	  (when (> (cadr (inner-rectangle obj))
		   (- (y obj) (vertical-speed obj)))
	    (setf (vertical-direction obj) :down)
	    (set-burning-marshmallow-animation obj)))))
     (move-about obj (make-xy
		      (if (eql (horizontal-direction obj) :left)
			  (- (horizontal-speed obj)) (horizontal-speed obj))
		      (if (eql (vertical-direction obj) :up)
			  (- (vertical-speed obj)) (vertical-speed obj)))))
    (T
     (dolist (player (get-objects *current-room* 'player))
       (if (and
	    (< (abs (- (x player) (x obj))) (+ +screen-width+ 300))
	    (< (abs (- (y player) (y obj))) (+ +screen-height+ 300)))
	   (setf (activated obj) T))))))

(defun set-burning-marshmallow-animation (obj)
  (cond
    ((eql (horizontal-direction obj) :LEFT)
     (cond
       ((eql (vertical-direction obj) :UP)
	(setf (animation obj) (slot-value obj 'lu-animation)))
       (T ;; (eql (vertical-direction obj) :DOWN)
	  (setf (animation obj) (slot-value obj 'ld-animation)))))
    (T ;;(eql (horizontal-direction obj) :RIGHT)
     (cond
       ((eql (vertical-direction obj) :UP)
	(setf (animation obj) (slot-value obj 'ru-animation)))
       (T ;; (eql (vertical-direction obj) :DOWN)
	(setf (animation obj) (slot-value obj 'rd-animation)))))))



(defun simple-enemy-and-player (player enemy)
  (decf (power player))
  (setf (active enemy) nil)
  (setf (visible enemy) nil)
  (setf (colliding enemy) nil))

(defmethod player-hits-enemy ((player player) (enemy burning-marshmallow) &rest args)
  (declare (ignore args))
  (decf (power player)))

(defmethod enemy-hits-player ((enemy burning-marshmallow) (player player) &rest args)
  (declare (ignore args))
  (decf (power player)))