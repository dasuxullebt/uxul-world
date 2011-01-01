;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defun i-wanna-listen-to (object room &rest args)
  "Brings all the Objects of the given classes in <args> into the
listen-to-array of object. Any previous value will be deleted."
  (dolist (arg args)
    (setf (listen-to object)
	  (concatenate 'list
		       (listen-to object)
		       (get-objects room arg)))))

(defun must-be-listened-by (object room &rest args)
  "Adds itself to the listen-to-array of all the objects of the given
classes in <args>"
  (dolist (arg args)
    (dolist (obj (get-objects room arg))
      (push object (listen-to obj)))))

(defgeneric add-object (obj place)
  (:documentation "Add an object to a place, i.e. a room or sth."))

(defmethod add-object ((obj t) (place t))
  "Just Warn - this shouldnt happen!"
  (format t
	  "add-object was called with arguments it wasnt defined
for. Classes: ~A ~A"
	  (class-name (class-of obj))
	  (class-name (class-of obj))))

(defmethod add-object ((object t) (room room))
  (add-object-of-class object (object-array room)))

(defmethod add-object ((obj stone) (place room))
  "Add a stone to a room and all the objects it can collide with"
  (must-be-listened-by obj place 'player 'moving-enemy 'moving-item)
  (call-next-method))

(defmethod add-object ((obj teleporter) (place room))
  (must-be-listened-by obj place 'player)
  (call-next-method))

(defmethod add-object ((obj moving-enemy) (place room))
  (i-wanna-listen-to obj place 'player 'stone)
  (must-be-listened-by obj place 'player)
  (call-next-method))

(defmethod add-object ((obj standing-enemy) (place room))
  (must-be-listened-by obj place 'player)
  (call-next-method))

(defmethod add-object ((obj moving-item) (place room))
  (must-be-listened-by obj place 'player)
  (i-wanna-listen-to obj place 'player 'stone)
  (call-next-method))

(defmethod add-object ((obj standing-item) (place room))
  (must-be-listened-by obj place 'player)
  (call-next-method))

(defmethod add-object ((obj player) (place room))
  (setf (key-listener place) obj)
  (setf (graphic-centralizer place) obj)
  (must-be-listened-by obj place 'moving-enemy 'moving-item)
  (i-wanna-listen-to obj place 'moving-enemy 'moving-item 'standing-enemy
		     'standing-item 'stone 'bottom)
  (call-next-method))