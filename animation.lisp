;;; Copyright 2009 Christoph Senjak

;; Basic definitions for animations. Needs lispbuilder-sdl.

(in-package :uxul-world)

(defparameter *zoom-ash* -1)
(defmacro zoom-trans (x) `(ash ,x *zoom-ash*))

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
   (images-2x :initarg :images-2x
	      :initform (make-array (list 0) :element-type 'sdl:surface)
	      :accessor images-2x
	      :documentation "Array of double-sized images")
   (images-1x :initarg :images-1x
	      :initform (make-array (list 0) :element-type 'sdl:surface)
	      :accessor images-1x
	      :documentation "Array of normal-sized images")
   (images-.5x :initarg :images-.5x
	       :initform (make-array (list 0) :element-type 'sdl:surface)
	       :accessor images-.5x
	       :documentation "Array of half-sized images")
   (images-.25x :initarg :images-.25x
		:initform (make-array (list 0) :element-type 'sdl:surface)
		:accessor images-.25x
		:documentation "Array of quarter-sized images")	      
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

(defmethod images ((obj animation))
  (cond
    ((= *zoom-ash* 0)
     (images-2x obj))
    ((= *zoom-ash* -1)
     (images-1x obj))
    ((= *zoom-ash* -2)
     (images-.5x obj))
    ((= *zoom-ash* -3)
     (images-.25x obj))))


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
    (sdl:draw-surface-at-* (elt (images obj) (sprite-image-number obj))
			   (zoom-trans (+ *current-translation-x* (round (x obj))))
			   (zoom-trans (+ *current-translation-y* (round (y obj)))))))

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

#|(defun load-png-image (filename)
  (sdl-image:load-image (gethash filename *file-table*) :image-type :PNG :alpha 1 )) ;; :alpha t))

(defun hashed-load-image (filename)
  "loads an image by its filename, if it wasnt loaded yet. returns a
reference, if the current filename already exists."
  (let ((ret (gethash filename *graphics-table* nil)))
    (cond
      (ret ret)
      (T
       (setf ret (load-png-image filename))
       (setf (gethash filename *graphics-table*) ret)
       ret))))|#

(defun make-animation (frame-skip &rest image-list)
  "Create an animation from the list of animation-names given in the
images-variable."
  (make-instance 'animation
		 :images-2x (mapcar
			     #'(lambda (x)
				 (sdl:convert-surface :surface (sdl-image:load-image
								(car x)
								:image-type :PNG :alpha 1 )))
			     image-list)
		 :images-1x (mapcar
			     #'(lambda (x)
				 (sdl:convert-surface :surface (sdl-image:load-image
								(cadr x)
								:image-type :PNG :alpha 1 )))
			     image-list)
		 :images-.5x (mapcar
			     #'(lambda (x)
				 (sdl:convert-surface :surface (sdl-image:load-image
								(caddr x)
								:image-type :PNG :alpha 1 )))
			     image-list)
		 :images-.25x (mapcar
			     #'(lambda (x)
				 (sdl:convert-surface :surface (sdl-image:load-image
								(cadddr x)
								:image-type :PNG :alpha 1 )))
			     image-list)
		 :sprite-delay frame-skip))