;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

;; as many game-objects do have an animation related to them, instead
;; of just having a draw-method which will manually draw anything, we
;; declare a standard-class for that, with some useful methods.

(defclass game-object-with-animation (game-object)
  ((animation-translation :initarg :animation-translation
			  :accessor animation-translation
			  :initform (make-xy 0 0)
			  :documentation "The translation of the animation")
   (animation :initarg :animation
	      :accessor animation
	      :documentation "The animation of this object")
   (animation-bounds :initarg :animation-bounds
		     :accessor animation-bounds
		     :initform (make-xy 50 50)
		     :documentation "When drawing, objects outside the
   screen are tried not to be drawn via SDL. This determines, how far
   in every direction the graphics may go outside the
   collision-rectangle. Try to keep this number small. If it is too
   huge, you may get numeric errors. 50/50 should be sufficient for
   most objects. If this value is nil, the object will always be
   drawn.")))

(defmethod (setf animation) ((newval animation) (obj game-object-with-animation))
  "Sets the animation and x and y-coordinates. Wont rewind the animation."
    (setf (slot-value obj 'animation) newval)
    (setf (x obj) (x obj))
    (setf (y obj) (y obj))
    (setf (visible obj) (visible obj)))

(defmethod (setf x) (newval (obj game-object-with-animation))
  (call-next-method)
  (setf (x (animation obj)) (+ (x obj) (x (animation-translation obj)))))

(defmethod (setf y) (newval (obj game-object-with-animation))
  (call-next-method)
  (setf (y (animation obj)) (+ (y obj) (y (animation-translation obj)))))

(defmethod (setf visible) (newval (obj game-object-with-animation))
  (call-next-method)
  (setf (visible (animation obj)) newval))

(defun rectangle-in-screen (obj)
  (let ((bounds (animation-bounds obj)))
    (if bounds
	(rectangles-overlap 
	 ;; HAAAAAAAAAAAAAAACK
	 (- (x obj) (x bounds))
	 (- (y obj) (y bounds))
	 (+ (x obj) (width obj) (x bounds))
	 (+ (y obj) (height obj) (y bounds))
	 (- *current-translation-x*)
	 (- *current-translation-y*)
	 (- +screen-width+ *current-translation-x*)
	 (- +screen-height+ *current-translation-y*))
	T)))



(defmethod draw ((obj game-object-with-animation))
  ;(if (rectangle-in-screen obj)
      (draw (animation obj))
;)
)

(defmethod shared-initialize :after ((instance game-object-with-animation) spam &rest
				     initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (declare (ignore spam))
  "Set the x and y-Coordinates in the drawable and the rectangle (this
had to be done by hand before)"
;  (write (x instance))
;  (write (y instance))
  (setf (x instance) (x instance))
  (setf (y instance) (y instance))
  (setf (visible instance) (visible instance)))

