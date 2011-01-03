;;; Copyright 2009-2011 Christoph Senjak

;; This file declares the constants for loading different files and
;; file-formats.

(in-package :uxul-world)

(defun si (var val)
  (setf (symbol-value (intern var)) val))

(defun stretch-image (x y img)
  "Resize that file to x times y."
  (uxul-world::resize-bmp-blob img x y))

;; (defun init-bmp-file (file)
;;   "Load an image file into a Variable. Set |filename| (without .png
;; and path) to a list with all sizes of that image."
;;   (si (pathname-name file)
;;        (with-open-file (in file :element-type '(unsigned-byte 8)) 
;; 	 (let* ((length (file-length in))
;; 		(content (make-array (list length)
;; 				     :element-type '(unsigned-byte 8)
;; 				     :adjustable nil)))
;; 	   (read-sequence content in)
;; 	   content))))

(defun bmp-p (file)
  "Is the file relevant for initialization? So far only .png-files are
relevant."
  (string= (pathname-type file) "bmp"))

;; (defun init-bmp-files ()
;;   (cl-fad:walk-directory
;;    (asdf:component-pathname (asdf:find-system :uxul-world))
;;    #'init-bmp-file :test #'bmp-p))

(defvar *spritesheet*)
(defvar *spritesheet-id*)
(defvar *buffer-id*)

(defun init-bmp-files ()
  (let* ((names (remove-if-not #'bmp-p
			       (cl-fad:list-directory
				(asdf:component-pathname
				 (asdf:find-system :uxul-world)))))
	 (number (length names))
	 (imagedata (mapcar #'load-file-to-sequence names))
	 (pixeldata (mapcar
		     (lambda (x)
		       (bmp-pixel-data x :destructive t))
		     imagedata))
	 (widths (mapcar #'bmp-width imagedata))
	 (heights (mapcar #'bmp-height imagedata))
	 (max-width (apply #'max widths))
	 (max-height (apply #'max heights))
	 ;; minimize max-height * optimal-x-num + max-width *
	 ;; optimal-y-num, keeping optimal-x-num * optimal-y-num
	 ;; constant at the number of files (of course, round
	 ;; everything up)
	 (optimal-x-num (ceiling
			 (sqrt (/ (* max-height number) max-width))))
	 (optimal-y-num (ceiling
			 (sqrt (/ (* max-width number) max-height))))
	 ;; find the smallest powers of two such that both fit in it
	 (sidelength (expt 2 (max
			      (ceiling (log (* max-width optimal-x-num) 2))
			      (ceiling (log (* max-height optimal-y-num) 2)))))
	 (new-image-data (make-array (list (* 4 sidelength sidelength))
				     :element-type '(unsigned-byte 8)
				     :adjustable nil
				     :initial-element #x00))
	 (cx 0) (cy 0)
	 (cx* 0) (cy* 0))
    (mapcar
     (lambda (name pixels width height)
       (blit-image cx* cy* width height pixels
		   sidelength sidelength new-image-data)
       (si (pathname-name name)
	   `(,width ,height
		    ,@(mapcar #'(lambda (x) (/ x sidelength 1.0))
			      (list cx* cy* (+ cx* width) (+ cy* height)))))
       (incf cx)
       (cond ((= cx optimal-x-num)
	      (incf cy)
	      (incf cy* max-height)
	      (setf cx 0 cx* 0))
	     (T (incf cx* max-width))))
     names pixeldata widths heights)
    (setf *spritesheet* (list new-image-data sidelength))))


(defun init-files ()
  "Load the relevant files into variables"
  (init-bmp-files))

(init-files)