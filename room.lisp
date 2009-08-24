;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defvar *current-room*)

(declaim (inline get-by-index))

(defun get-by-index (index array)
  (svref array index))

(defun create-object-array ()
  (make-array (list (length +class-indices+))
	      :element-type 'list
	      :initial-element nil
	      :adjustable nil))

(defun add-object-of-class (object array)
  (dolist (class (c2mop:class-precedence-list (class-of object)))
    (let ((index (position (class-name class) +class-indices+)))
      (if index
	  (pushnew object (svref array index))))))

(defun get-objects-of-class (class-name array)
  (get-by-index (position class-name +class-indices+) array))


(define-compiler-macro get-objects-of-class (&whole form class-name array)
   (if (constantp class-name)
       `(get-by-index ,(position (eval class-name) +class-indices+) ,array)
       form))

(defun get-objects (room class)
  (get-objects-of-class class (object-array room)))

(define-compiler-macro get-objects (&whole form room class-name)
  (format t "Compiler Macro for get-objects...")
  (print (if (constantp class-name)
      `(get-by-index ,(position (eval class-name) +class-indices+) (object-array ,room))
      form
      )))

(defclass room ()
  ((key-down-function :initform
		      #'(lambda (key) (declare (ignore key)))
		      :accessor key-down-function
		      :initarg :key-down-function
		      :documentation "Function to call in case of a
		      key-down event.")
   (key-up-function :initform
		    #'(lambda (key) (declare (ignore key)))
		    :accessor key-up-function
		    :initarg :key-up-function
		    :documentation "Function to call in case of a
		    key-up event.")
   (object-array :initform (create-object-array)
		 :accessor object-array
		 :initarg :object-array
		 :documentation "Array of Objects indexed by class.")
   (key-listener :initarg :key-listener
		 :accessor key-listener
		 :documentation "An Object with Methods on-key-up and
		 on-key-down, to which key-events are passed.")
   (graphic-centralizer :initarg :graphic-centralizer
			:accessor graphic-centralizer)
   (background-surface :initarg :background-surface
		       :accessor background-surface)
   (background-surface-drawn :initarg :background-surface-drawn
			     :accessor background-surface-drawn
			     :initform nil)
   (invocation-function :initform nil
			:accessor invocation-function
			:documentation "Will be called, if not nil, by
   invoke, so 'overriding' the invoke-method for room (implemented for
   Pausings, etc.). Set to nil, the normal invoke-method will be
   called again.")
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (position-table :initarg :position-table :accessor position-table
		   :initform (make-hash-table :test 'eql)
		   :documentation ":tblabla-Symbols in
   make-tiled-room are pushed as keys with the associated
   positions to this table.")))

(defmethod on-key-down ((obj room) key)
  (on-key-down (key-listener obj) key))

(defmethod on-key-up ((obj room) key)
  (on-key-up (key-listener obj) key))

(defmethod invoke ((obj room))
  (if (invocation-function obj)
      (funcall (invocation-function obj) obj)
      (dolist (invoker (get-objects obj 'uxul-world::game-object))
	(if (active invoker) (invoke invoker)))))

(defun create-room-from-item-list (item-list)
  (let*
      ((player (make-instance 'player
				:active t
				:visible t
				:redraw t))
       (room (make-instance 'room :width 0 :height 0
			    :graphic-centralizer player
			    :key-listener player
			    :key-up-function #'(lambda (key) (on-key-up player key))
			    :key-down-function #'(lambda (key) (on-key-down player key))))
       (anchor-table (make-hash-table :test 'equal)))
    (dolist (item item-list)
      (let ((y (car item))
	    (x (cadr item))
	    (type (caddr item))
	    (arg1 (cadddr item)))
	(when (eq type 'anchor)
	  (setf (gethash arg1 anchor-table) (cons x y)))))
    (dolist (item item-list)
      (let ((y (car item))
	    (x (cadr item))
	    (type (caddr item))
	    (arg1 (cadddr item))
	    (arg2 (car (cddddr item))))
	(cond
	  ((eq type 'anchor))
	  ((eq type 'uxul)
	   (setf (x player) (* 128 x))
	   (setf (y player) (* 128 y))
	   (add-object player room))
	  ((eq type 'tulip)
	   (add-object (make-instance 'tulip
				      :x (* 128 x)
				      :y (* 128 y)) room))
	  ((eq type 'brown-stone)
	   (add-object (make-instance 'stone
				      :animation (make-animation 0 |brown_stone|)
				      :x (* 128 x)
				      :y (* 128 y)) room))
	  ((eq type 'gray-stone)
	   (add-object (make-instance 'stone
				      :animation (make-animation 0 |gray_stone|)
				      :x (* 128 x)
				      :y (* 128 y)) room))
	  ((eq type 'nasobem)
	   (add-object (make-instance 'simple-enemy
				      :x (* 128 x)
				      :y (* 128 y)) room))
	  ((eq type 'blue-nasobem)
	   (add-object (make-instance 'flying-nasobem
				      :x (* 128 x)
				      :y (* 128 y)) room))
	  ((eq type 'burning-marshmallow)
	   (add-object (make-instance 'burning-marshmallow
				      :x (* 128 x)
				      :y (* 128 y)
				      :inner-rectangle
				      (and (not (string= arg1 "")) (not (string= arg2 ""))
					   (list
					    (* 128 (car (gethash arg1 anchor-table)))
					    (* 128 (cdr (gethash arg1 anchor-table)))
					    (* 128 (1+ (car (gethash arg2 anchor-table))))
					    (* 128 (1+ (cdr (gethash arg2 anchor-table))))))) room))
	  (T
	   (add-object (make-instance type
				      :x (* 128 x)
				      :y (* 128 y)) room)))))
    room))