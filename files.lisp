;;; Copyright 2009 Christoph Senjak

;; This file declares the constants for loading different files and
;; file-formats.

(in-package :uxul-world)

(defun si (var val)
  (setf (symbol-value (intern var)) val))


(defun stretch-image (x y img)
  "Call ImageMagick to resize that file to 64x64."
  (lisp-magick:with-magick-wand (mywand)
    (lisp-magick::magick-read-image-blob mywand img)
    (lisp-magick::magick-resize-image mywand x y #x00000000 1d0)
    (lisp-magick::magick-set-format mywand "png")
    (lisp-magick::magick-get-image-blob mywand)))

(defun ash-sized-image (img a)
  "Calculate an image of half of the size."
  (lisp-magick:with-magick-wand (mywand)
    (lisp-magick::magick-read-image-blob mywand img)
    (let
	((w (lisp-magick::magick-get-image-width mywand))
	 (h (lisp-magick::magick-get-image-height mywand)))
      (lisp-magick::magick-resize-image mywand
					(max 1 (floor (/ w a))) (max 1 (floor (/ h a))) ;; no ash here ...
					#x00000000 1d0)
      (lisp-magick::magick-set-format mywand "png")
      (lisp-magick::magick-get-image-blob mywand))))

(defun all-sizes (img)
  (list img
	(ash-sized-image img 2)
	(ash-sized-image img 4)
	(ash-sized-image img 8)))


;; (defun init-file (file)
;;   "Load a file into a Variable. Access with |filename| (without .png
;; and path)."
;;   (si (pathname-name file)
;;       (stretched-image 
;;       (with-open-file (in file :element-type '(unsigned-byte 8)) 
;; 	(let* ((length (file-length in))
;; 	       (content (make-array (list length)
;; 				    :element-type '(unsigned-byte 8)
;; 				    :adjustable nil)))
;; 	  (read-sequence content in)
;; 	  content)))))

(defun init-png-file (file)
  "Load an image file into a Variable. Set |filename| (without .png
and path) to a list with all sizes of that image."
  (si (pathname-name file) 
      (all-sizes
       (with-open-file (in file :element-type '(unsigned-byte 8)) 
	 (let* ((length (file-length in))
		(content (make-array (list length)
				     :element-type '(unsigned-byte 8)
				     :adjustable nil)))
	   (read-sequence content in)
	   content)))))

(defun png-p (file)
  "Is the file relevant for initialization? So far only .png-files are
relevant."
  (string= (pathname-type file) "png"))

(defun init-png-files ()
  (cl-fad:walk-directory
   (asdf:component-pathname (asdf:find-system :uxul-world))
   #'init-png-file :test #'png-p))

(defun init-files ()
  "Load the relevant files into variables"
  (init-png-files))

(init-files)