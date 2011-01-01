;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

;; Define a class for the Standard Game-Object which has a draw-Method
;; which will be called at every frame, and a Collision-Box, and has a
;; unique (x, y)-Coordinate with translations for both the Drawable
;; Object and the collision-box

;; We changed the api, and added stuff from Collision-Rectangle (which
;; explains some documentation about it)

(defclass game-object (xy-coordinates)
  ((width :initarg :width
	  :initform 0
	  :accessor width
	  :type fixnum
	  :documentation "The width of that rectangle")
   (height :initarg :height
	   :initform 0
	   :accessor height
	   :type fixnum
	   :documentation "The height of that rectangle")
   (listen-to :initarg :listen-to
	      :initform NIL
	      :accessor listen-to
	      :documentation "List of rectangles and game-objects to
	      check for collisions at movement")
   (colliding :initarg :colliding
	      :initform T
	      :accessor colliding
;	      :type boolean
	      :documentation "Throw Collisions with this
	      Collision-Rectangle to other Collision-Rectangles? (this
	      makes it a bit easier to \"turn off\" Objects, i.e. you
	      dont always have to remove them from the
	      listen-to-lists")
   (visible :initarg :visible
	    :initform T
	    :accessor visible
;	    :type boolean
	    :documentation "Should this Object be drawn?")
   (redraw :initarg :redraw
	   :initform T
	   :accessor redraw
	   :documentation "If set to nil, this object will be painted
	   once onto the Background of the Level and then never be
	   painted again (except when needed), i.e. the engine first
	   paints it onto its background-surface, and then it keeps
	   using its background-surface for all further images. This
	   makes drawing faster. It should be set to NIL whenever
	   possible, however, if the Object will change its place or
	   look different in the future, or should be painted over
	   some other object that can move or change its look, then it
	   must be set to T, because it must be redrawn. NOTICE: It is
	   not specified, what happens, if this Value changes during
	   runtime. It should not be set manually after it is used by
	   the engine.

**********************FIXME: DOESNT WORK ATM**********************

")
   (active :initarg :active
	   :initform NIL
	   :accessor active
;	   :type boolean
	    :documentation "Will the Invoke-Function be called?")
   (object-id :initarg :object-id
	      :initform NIL
	      :accessor object-id
	      :documentation "To identify an object, a room may give it an id."))
   (:documentation "Define a Class for all Game-Objects. This class
   has an invoke-, a draw- and an on-collide Function, which do
   nothing per default." ))

(defmethod draw ((obj game-object))
  "To be called when drawing the object - does nothing per default, except throwing a warning."
  (format t "waring: draw-method not overridden. Object: ")
  (write obj)
  (sdl:push-quit-event))

(defmethod invoke ((obj game-object))
  "To be called when invoking the object - does nothing per default, except throwing a warning."
  (format t "warning: invoke-method not overridden. Object: ")
  (write obj)
  (sdl:push-quit-event))

(defmethod on-collision ((moving-object game-object) (standing-object game-object) (collision collision))
  "To be called if a Collision occurs. May have more than one overriding declaration, to use the dispatcher."
  (declare (ignore standing-object moving-object collision))
  (format t "warning: on-collision-method not overridden."))

(defmethod half-width ((obj game-object))
  (/ (width obj) 2))
(defmethod (setf half-width) (x (obj game-object))
  (setf (width obj) (* x 2)))
(defmethod half-height ((obj game-object))
  (/ (height obj) 2))
(defmethod (setf half-height) (x (obj game-object))
  (setf (height obj) (* x 2)))

(defmethod mid-x ((obj game-object))
  (+ (x obj) (half-width obj)))

(defmethod mid-y ((obj game-object))
  (+ (y obj) (half-height obj)))

(defmethod (setf mid-x) (x (obj game-object))
  (setf (x obj) (- x (half-width obj))))

(defmethod (setf mid-y) (y (obj game-object))
  (setf (y obj) (- y (half-height obj))))

(defmethod move-about ((moving-rectangle game-object) (translation xy-struct))
  (if (= (x translation) 0)
      (when (not (= (y translation) 0))
	(move-collision-rectangle-about-y moving-rectangle (y translation)))
      (if (= (y translation) 0)
	  (move-collision-rectangle-about-x moving-rectangle (x translation))
	  (move-collision-rectangle-about-xy moving-rectangle (x translation) (y translation)))))


(defmethod move-to ((moving-rectangle game-object) (translation xy-struct))
  "This is highly inefficient and should be replaced"
  (move-about moving-rectangle
	      (make-xy (- (x translation) (x moving-rectangle)) (- (y translation) (y moving-rectangle)))))


(defmethod draw-bounds ((obj game-object))
  "This function draws a rectangle with the Object's Bounds. May be useful for some debug-spam"
  ;; (sdl:draw-rectangle-* (+ (x obj) *current-translation-x*)
  ;; 			(+ (y obj) *current-translation-y*)
  ;; 			(width obj) (height obj)
  ;; 			:color sdl:*BLACK*)
)

(defun collide-blocks (moving-rectangle standing-rectangle collision)
  "as MANY collision-methods need to move the moving-object around the
standing-object, we will write a function for doing that. IMPORTANT:
moving-rectangle MUST have a dont-ignore-property"
  (declare (ignore standing-rectangle))
  (directly-with-all-accessors collision collision
    (setf (x moving-rectangle) (x pos))
    (setf (y moving-rectangle) (y pos))
    (cond
      ((or (eq direction :left) (eq direction :right))
       (move-about moving-rectangle (make-xy 0 (truncate (* (- 1 collision-time) (y desired-movement))))))
      ((or (eq direction :up) (eq direction :down))
       (move-about moving-rectangle (make-xy (truncate (* (- 1 collision-time) (x desired-movement))) 0)))
      (T ;; diagonal - argh! lets try to move up/down. if this fails,
       ;; lets try to move left/right. we're setting our
       ;; dont-ignore-flag to nil for that
       (let ((current-y (y moving-rectangle))
	     (current-x (x moving-rectangle)))
	 (setf (dont-ignore moving-rectangle) nil)
	 (move-about moving-rectangle (make-xy (truncate (* (- 1 collision-time) (x desired-movement))) 0))
	 (if (not (= current-x (x moving-rectangle)))
	     (progn
	       (setf (x moving-rectangle) current-x)
	       (setf (dont-ignore moving-rectangle) T)
	       ;; now really move it!
	       (move-about moving-rectangle (make-xy (truncate (* (- 1 collision-time) (x desired-movement))) 0)))
					;else - it cannot move in x-direction...
	     (progn
	       (move-about moving-rectangle (make-xy 0 (truncate (* (- 1 collision-time) (y desired-movement)))))
	       (when (not (= current-y (y moving-rectangle)))
		 (setf (y moving-rectangle) current-y)
		 (setf (dont-ignore moving-rectangle) T)
		 ;; now really move it!
		 (move-about moving-rectangle (make-xy 0 (truncate (* (- 1 collision-time) (y desired-movement)))))))))))))
