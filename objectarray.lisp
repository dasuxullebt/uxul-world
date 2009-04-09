;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

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

;; for rooms

(defmethod add-object ((room room) (object t))
  (add-object-of-class object (object-array room)))

(defun get-objects (room class)
  (get-objects-of-class class (object-array room)))

(define-compiler-macro get-objects (&whole form room class-name)
  (format t "Compiler Macro for get-objects...")
  (print (if (constantp class-name)
      `(get-by-index ,(position (eval class-name) +class-indices+) (object-array ,room))
      form
      )))
