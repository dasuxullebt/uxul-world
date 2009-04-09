;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(declaim (inline make-xy coordinate-distance))

(defstruct xy-struct (x 0 :type fixnum) (y 0 :type fixnum))

(defmethod x ((obj xy-struct)) (slot-value obj 'x))
(defmethod (setf x) (new-value (obj xy-struct))
  (setf (slot-value obj 'x) (the number new-value)))
(defmethod y ((obj xy-struct)) (slot-value obj 'y))
(defmethod (setf y) (new-value (obj xy-struct))
  (setf (slot-value obj 'y) (the number new-value)))

(defclass xy-coordinates ()
  ((x :accessor x :initarg :x :initform 0 :type fixnum)
   (y :accessor y :initarg :y :initform 0 :type fixnum)))

(defun coordinate-distance (a b)
  "Calculate the euklidian distance of two points. They must have x-
and y-accessors."
  (sqrt (+ (expt (- (x a) (x b)) 2) (expt (- (y a) (y b)) 2))))

(defun make-xy (x y)
  (declare (type fixnum x y))
  "Guess what this function does..."
  (make-xy-struct :x x :y y))