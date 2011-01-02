;;; Copyright 2009-2011 Christoph Senjak

;; Basic definitions for animations.

(in-package :uxul-world)

(defparameter *graphics-table* nil)

;; the functions may assume that the contents of a graphics-file -
;; once read - will not change at any time - so it wont reload
;; graphics with an equivalent path any time you load an image.

(defclass animation (xy-coordinates)
  (
;;   (images :initarg :images
;; 		     :initform (make-array (list 0) :element-type 'sdl:surface)
;; 		     :accessor images
;; ;		     :type (simple-array 'sdl:surface (*))
;; 		     :documentation "Array with the images")
   (full-widths :initarg :full-widths
		:initform (make-array (list 0))
		:accessor full-widths
		:documentation "Widths of images")
   (full-heights :initarg :full-heights
		:initform (make-array (list 0))
		:accessor full-heights
		:documentation "Heights of images")
   (images :initarg :images
	   :initform (make-array (list 0))
	   :accessor images
	   :documentation "Array of images")
   (sprite-image-number :initform 0
			:initarg :sprite-image-number
			:accessor sprite-image-number
;			:type xy-struct
			:documentation "The Element-Number of the
		       current image. This slot should not be set
		       directly.")
   (sprite-delay :initarg :sprite-delay
		 :initform 0
		 :accessor sprite-delay
;		 :type integer
		 :documentation "How much frames to overjump on the
whole until changing to the next image of the animation.")
   (already-jumped :initform 0
		   :initarg :already-jumped
		   :accessor already-jumped
;		   :type integer
		   :documentation "How much frames have been already
   drawn until the last jump? If this equals to <sprite-delay>, the
   next image is selected. Dont set this variable yourself." )
   (visible :initarg :visible
	    :initform T
	    :accessor visible
;	    :type boolean
	    :documentation "Should this Animation be visible (i.e. be
            drawn when the draw-method is called)? Anyway, the
            draw-method will - even if set to false - \"animate\" the
            animation, i.e. rotate the image currently drawn, if not
            paused. It simply wont draw the graphics to the
            screen.")
   (reference-to-original :initarg :reference-to-original
			  :accessor reference-to-original
			  :initform nil
			  :documentation "DO NOT SET THIS MANUALLY! DO
NOT USE IT! This may not stay in later versions of this Program. It
will be used to minimize the number of file-accesses for loading
animations. For any animation created from a file by the api from
below, this will refer to an animation in the *graphics-table*." )))

(defmethod draw ((obj animation))
  (when (not (<= (sprite-delay obj) 0)) ;<=, because -a means "paused,
					;but a is the delay when
					;playing again", and 0 means
					;"no playing"
    (incf (already-jumped obj))
    (when (= (sprite-delay obj) (already-jumped obj))
      (setf (already-jumped obj) 0)
      (setf (sprite-image-number obj) (mod (+ 1 (sprite-image-number obj)) (length (images obj))))))
  (when (visible obj)
    (make-quad (elt (images obj) (sprite-image-number obj))
	       (round (x obj))
	       (round (y obj))
	       (elt (full-widths obj) (sprite-image-number obj))
	       (elt (full-heights obj) (sprite-image-number obj)))))

;additional methods to make life easier
(defmethod pause ((obj animation))
  "toggle the playing-flag (sgn sprite-delay), see documentation of draw-method."
  (setf (sprite-delay obj) (- (sprite-delay obj))))

(defmethod is-paused ((obj animation))
  "is animation paused?"
  (< (sprite-delay obj) 0))

(defmethod is-playing ((obj animation))
  "is animation playing?"
  (< 0 (sprite-delay obj)))

(defmethod ensure-pause ((obj animation))
  "ensures that the animation is paused if playing, otherwise, nothing is done."
  (when (is-playing obj) (pause obj)))

(defmethod ensure-playing ((obj animation))
  "ensures that the animation is playing if paused, otherwise, nothing is done."
  (when (is-paused obj) (pause obj)))

(defmethod rewind ((obj animation))
  "rewind the animation"
  (setf (slot-value obj 'sprite-image-number) 0))

(defun make-animation (frame-skip &rest image-list)
  "Create an animation from the list of animation-names given in the
images-variable."
  ;(format t "make-animation is being called~%")
  (make-instance 'animation
		 :full-widths (mapcar #'car image-list)
		 :full-heights (mapcar #'cadr image-list)
		 :images (mapcar #'cddr image-list)
		 :sprite-delay frame-skip))