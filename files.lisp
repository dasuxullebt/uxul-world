;;; Copyright 2009-2011 Christoph Senjak

;; This file declares the constants for loading different files and
;; file-formats.

(in-package :uxul-world)

(defun si (var val)
  (setf (symbol-value (intern var)) val))

(defun stretch-image (x y img)
  "Resize that file to x times y."
  (uxul-world::resize-bmp-blob img x y))

(defun ash-sized-image (img a)
  "Calculate an image of half/eighth/quarter of the size."
  (let ((w (bmp-width img))
	(h (bmp-height img)))
  (uxul-world::resize-bmp-blob img (max 1 (floor (/ w a))) (max 1 (floor (/ h a))))))

(defun init-bmp-file (file)
  "Load an image file into a Variable. Set |filename| (without .png
and path) to a list with all sizes of that image."
  (si (pathname-name file)
       (with-open-file (in file :element-type '(unsigned-byte 8)) 
	 (let* ((length (file-length in))
		(content (make-array (list length)
				     :element-type '(unsigned-byte 8)
				     :adjustable nil)))
	   (read-sequence content in)
	   content))))

(defun bmp-p (file)
  "Is the file relevant for initialization? So far only .png-files are
relevant."
  (string= (pathname-type file) "bmp"))

(defun init-bmp-files ()
  (cl-fad:walk-directory
   (asdf:component-pathname (asdf:find-system :uxul-world))
   #'init-bmp-file :test #'bmp-p))

(defun init-files ()
  "Load the relevant files into variables"
  (init-bmp-files))

(init-files)