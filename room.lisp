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
	  ((eq type 'key)
	   (add-object (make-instance 'key
				      :x (* 128 x)
				      :y (* 128 y)
				      :dungeon arg1) room))
	  ((eq type 'door)
	   (add-object (make-instance 'door
				      :x (* 128 x)
				      :y (* 128 y)
				      :dungeon arg1) room))
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

(defparameter *additional-testing-room*
  '((14 8 NASOBEM "" "") (3 9 BURNING-MARSHMALLOW "" "")
    (5 10 DOOR "" "")
    (5 14 BROWN-STONE "" "") (5 13 BROWN-STONE "" "") (5 12 BROWN-STONE "" "")
    (5 11 BROWN-STONE "" "") (5 6 BROWN-STONE "" "") (5 9 BROWN-STONE "" "")
    (5 8 BROWN-STONE "" "") (5 7 BROWN-STONE "" "") (7 14 TULIP "" "")
    (1 2 KEY "" "") (1 1 TULIP "" "") (2 3 DOOR "" "") (3 4 BROWN-STONE "" "")
    (2 4 BROWN-STONE "" "") (1 4 BROWN-STONE "" "") (2 2 BROWN-STONE "" "")
    (2 1 BROWN-STONE "" "") (4 3 BROWN-STONE "" "") (4 2 BROWN-STONE "" "")
    (7 6 DOOR "" "") (11 3 DOOR "" "") (5 1 KEY "" "") (5 3 BROWN-STONE "" "")
    (5 2 DOOR "" "") (5 4 BROWN-STONE "" "") (5 5 BROWN-STONE "" "")
    (6 5 BROWN-STONE "" "") (7 5 BROWN-STONE "" "") (7 3 BROWN-STONE "" "")
    (6 1 BROWN-STONE "" "") (7 2 BROWN-STONE "" "") (7 1 TULIP "" "")
    (9 3 BROWN-STONE "" "") (9 2 BROWN-STONE "" "") (11 1 BROWN-STONE "" "")
    (11 2 BROWN-STONE "" "") (13 9 BROWN-STONE "" "") (11 5 DOOR "" "")
    (9 14 KEY "" "") (9 12 KEY "" "") (8 14 BROWN-STONE "" "")
    (9 13 BROWN-STONE "" "") (8 13 BROWN-STONE "" "") (7 13 BROWN-STONE "" "")
    (7 12 BROWN-STONE "" "") (7 11 BROWN-STONE "" "") (7 10 BROWN-STONE "" "")
    (7 9 BROWN-STONE "" "") (7 8 BROWN-STONE "" "") (7 7 BROWN-STONE "" "")
    (8 7 BROWN-STONE "" "") (9 7 BROWN-STONE "" "") (9 6 BROWN-STONE "" "")
    (9 5 BROWN-STONE "" "") (9 4 BROWN-STONE "" "") (10 4 BROWN-STONE "" "")
    (11 4 BROWN-STONE "" "") (11 6 BROWN-STONE "" "") (11 7 BROWN-STONE "" "")
    (11 8 BROWN-STONE "" "") (10 10 KEY "" "") (10 11 BROWN-STONE "" "")
    (9 11 BROWN-STONE "" "") (9 10 BROWN-STONE "" "") (9 9 BROWN-STONE "" "")
    (10 9 BROWN-STONE "" "") (11 9 BROWN-STONE "" "") (12 12 KEY "" "")
    (11 14 BROWN-STONE "" "") (11 13 BROWN-STONE "" "") (11 12 BROWN-STONE "" "")
    (11 11 BROWN-STONE "" "") (12 11 BROWN-STONE "" "") (13 13 BROWN-STONE "" "")
    (13 12 BROWN-STONE "" "") (13 11 BROWN-STONE "" "") (13 10 BROWN-STONE "" "")
    (13 8 BROWN-STONE "" "") (13 7 BROWN-STONE "" "") (13 6 BROWN-STONE "" "")
    (13 5 BROWN-STONE "" "") (13 4 BROWN-STONE "" "") (13 3 BROWN-STONE "" "")
    (13 2 BROWN-STONE "" "") (14 1 UXUL "" "") (0 14 BROWN-STONE "" "")
    (0 13 BROWN-STONE "" "") (0 12 BROWN-STONE "" "") (0 11 BROWN-STONE "" "")
    (0 10 BROWN-STONE "" "") (0 9 BROWN-STONE "" "") (0 8 BROWN-STONE "" "")
    (0 7 BROWN-STONE "" "") (0 6 BROWN-STONE "" "") (0 5 BROWN-STONE "" "")
    (0 4 BROWN-STONE "" "") (0 3 BROWN-STONE "" "") (0 2 BROWN-STONE "" "")
    (0 1 BROWN-STONE "" "") (0 0 BROWN-STONE "" "") (1 0 BROWN-STONE "" "")
    (2 0 BROWN-STONE "" "") (3 0 BROWN-STONE "" "") (6 0 BROWN-STONE "" "")
    (5 0 BROWN-STONE "" "") (4 0 BROWN-STONE "" "") (7 0 BROWN-STONE "" "")
    (8 0 BROWN-STONE "" "") (9 0 BROWN-STONE "" "") (10 0 BROWN-STONE "" "")
    (11 0 BROWN-STONE "" "") (12 0 BROWN-STONE "" "") (13 0 BROWN-STONE "" "")
    (14 0 BROWN-STONE "" "") (8 15 BROWN-STONE "" "") (7 15 BROWN-STONE "" "")
    (5 15 BROWN-STONE "" "") (6 15 BROWN-STONE "" "") (4 15 BROWN-STONE "" "")
    (3 15 BROWN-STONE "" "") (2 15 BROWN-STONE "" "") (1 15 BROWN-STONE "" "")
    (0 15 BROWN-STONE "" "") (9 15 BROWN-STONE "" "") (10 15 BROWN-STONE "" "")
    (11 15 BROWN-STONE "" "") (12 15 BROWN-STONE "" "") (13 15 BROWN-STONE "" "")
    (14 15 BROWN-STONE "" "") (15 15 BROWN-STONE "" "") (15 14 BROWN-STONE "" "")
    (15 13 BROWN-STONE "" "") (15 12 BROWN-STONE "" "") (15 11 BROWN-STONE "" "")
    (15 10 BROWN-STONE "" "") (15 9 BROWN-STONE "" "") (15 8 BROWN-STONE "" "")
    (15 7 BROWN-STONE "" "") (15 6 BROWN-STONE "" "") (15 5 BROWN-STONE "" "")
    (15 4 BROWN-STONE "" "") (15 3 BROWN-STONE "" "") (15 2 BROWN-STONE "" "")
    (15 1 BROWN-STONE "" "") (15 0 BROWN-STONE "" "")))
(defun make-additional-testing-room ()
  (let
      ((room (create-room-from-item-list *additional-testing-room*)))
    (add-object (make-instance 'teleporter
			       :next-room-function #'make-testing-room
			       :x (* 128 9) :y (* 128 14)
			       :active nil :redraw T :visible T :colliding T) room)
    room))