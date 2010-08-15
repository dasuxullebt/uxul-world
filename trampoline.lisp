;;; Copyright 2010 Christoph Senjak

(in-package :uxul-world)

(defclass trampoline (game-object-with-animation)
  ((dont-ignore :accessor dont-ingnore :initform t)
   (width :initarg :width :initform 64 :accessor width)
   (height :initarg :height :initform 64