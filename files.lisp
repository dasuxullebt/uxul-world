;;; Copyright 2009 Christoph Senjak

;; This file declares the constants for loading different files and
;; file-formats.

(in-package :uxul-world)

(defun si (var val)
  (setf (symbol-value (intern var)) val))

(defun init-file (file)
  "Load a file into a Variable. Access with |filename| (without .png
and path)."
  (si (pathname-name file)
      (with-open-file (in file :element-type '(unsigned-byte 8)) 
	(let* ((length (file-length in))
	       (content (make-array (list length)
				    :element-type '(unsigned-byte 8)
				    :adjustable nil)))
	  (read-sequence content in)
	  content))))

(defun file-relevant-p (file)
  "Is the file relevant for initialization? So far only .png-files are
relevant."
  (string= (pathname-type file) "png"))

(defun init-files ()
  "Load the relevant files into variables"
  (cl-fad:walk-directory
   (asdf:component-pathname (asdf:find-system :uxul-world))
   #'init-file :test #'file-relevant-p))

(init-files)