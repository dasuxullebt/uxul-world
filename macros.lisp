;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)

(defvar *current-translation-x* 0)
(defvar *current-translation-y* 0)
(defmacro with-translation-* ((x y) &body body)
  `(let ((*current-translation-x* (+ ,x *current-translation-x*))
	 (*current-translation-y* (+ ,y *current-translation-y*)))
     ,@body))

(defmacro with-translation ((translation) &body body)
  `(with-translation-* ((x ,translation) (y ,translation)) ,@body))

(defmacro with-negative-translation-* ((x y) &body body)
  `(with-translation-* ((- ,x) (- ,y)) ,@body))

(defmacro with-negative-translation ((translation) &body body)
  `(with-negative-translation-* ((x ,translation) (y ,translation)) ,@body))

(defmacro directly-with-accessors (accessors objname &body body)
	       `(with-accessors (
				 ,@(let ((args nil))
				   (dolist (arg accessors args)
				     (push (list arg arg) args))))
		    ,objname ,@body))

(defun class-all-readers (class)
          (nconc (loop for superclass in 
		      (closer-mop:class-direct-superclasses class)
                       nconc (class-all-readers superclass))
                 (loop for direct-slot in
		      (closer-mop:class-direct-slots class)
		    append 
		      (closer-mop:slot-definition-readers direct-slot))))

(defmacro directly-with-all-accessors (classname objname &body body)
  `(directly-with-accessors (,@(class-all-readers (find-class classname)))
      ,objname ,@body))

(defmacro defvars (&rest vars)
  `(progn
     ,@(let ((ret nil))
	    (dolist (var vars ret)
	      (push `(defvar ,var) ret)))))


(defmacro let-accessor (((accessor object) value) &body body)
  "Temporarily set an Accessor to another value."
  (let ((symbol (gensym)))
    `(let ((,symbol (,accessor ,object)))
       (unwind-protect
	    (progn (setf (,accessor ,object) ,value) ,@body)
	 (setf (,accessor ,object) ,symbol)))))

(defmacro let-accessors ((&rest bindings) &body body)
  "Temporarily set Accessors to other values."
  (let ((cbind (car bindings)))
    (if cbind
	`(let-accessor
	     ((,(first (first cbind)) ,(second (first cbind))) ,(second cbind))
	   (let-accessors (,@(cdr bindings)) ,@body))
	`(progn ,@body))))