;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

;;; Desired behaviour: Fly around, and if uxul gets near, try to crash
;;; it. When Uxul jumps on top of it, then get broken.


(defclass flying-nasobem (simple-enemy)
  ((animation :initarg :animation
	      :initform
	      (make-animation 3 |blue_nasobem| |blue_nasobem2|)
	      :accessor animation)
   (animation-translation :accessor animation-translation
			  :initarg :animation-translation
			  :initform (make-xy -100 -50))

   (flat-animation :initform (make-animation 0 |blue_nasobem3|)
		   :accessor flat-animation)

   (invoke-continuation :initform #'invoke-flying-nasobem)
   (dont-ignore :accessor dont-ignore :initform t)
   (width :initarg :width :initform 64 :accessor width)
   (active :initarg :active :initform t :accessor active)
   (height :initarg :height :initform 64 :accessor height)
   (direction :initarg :direction :initform :left :accessor direction)))

(defun invoke-flying-nasobem-wait (flying-nasobem frames)
  (if (zerop frames)
      #'invoke-flying-nasobem
      #'(lambda (fn) (invoke-flying-nasobem-wait fn (1- frames)))))

(defun invoke-flying-nasobem-playerhunt (flying-nasobem x y maxtime)
  (move-about flying-nasobem
	      (make-xy
	       (if (< (x flying-nasobem) x) 20 -20)
	       (if (< (y flying-nasobem) y) 20 -20)))
  (if (zerop maxtime)
	   #'(lambda (k) (invoke-flying-nasobem-wait k 16))
	   #'(lambda (k) (invoke-flying-nasobem-playerhunt 
			  k x y (1- maxtime)))))


(defun invoke-flying-nasobem (flying-nasobem)
  (block return-here
    (dolist (player (get-objects *current-room* 'player))
      (if (and
	   (< (abs (- (x player) (x flying-nasobem))) 700)
	   (< (abs (- (y player) (y flying-nasobem))) 700))
	  (return-from return-here #'(lambda (fn)
				       (invoke-flying-nasobem-playerhunt
				       fn (x player) (y player) 25)))
	  (return-from return-here #'invoke-flying-nasobem)))))

      


(defmethod invoke ((obj flying-nasobem))
  (setf (slot-value obj 'invoke-continuation)
	(funcall (slot-value obj 'invoke-continuation) obj)))
