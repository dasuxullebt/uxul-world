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