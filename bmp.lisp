;;; -*- lisp -*-

;;; Copyright 2010-2011 Christoph Senjak

(in-package :uxul-world)

(defun intersection-interval (a b c d)
  "We assume a<b and c<d. Compute the intersection-interval between
[a;b] and [c;d]. Return three values: 1. generalized boolean whether
they intersect. 2. lower bound, 3. upper bound"
  (let
      ((r-a (max a c)) (r-b (min b d)))
    (values (< r-a r-b) r-a r-b)))

(defun intersection-rectangle (x1 y1 x2 y2 x3 y3 x4 y4)
  "Compute the intersection-rectangle between the rectangle with
diagonal points (x1,y1) and (x2, y2) and the one with (x3,
y3),(x4,y4)?  Assuming x1<x2, x3<x4, y same. Return five values x a b
c d, where x is a generalized boolean whether they overlap at all,
(a b) is the upper left and (c d) is the lower right edge of the
rectangle, or nil if they dont intersect."
  (multiple-value-bind
	(ov1 ix1 ix2) (intersection-interval x1 x2 x3 x4)
    (multiple-value-bind
	  (ov2 iy1 iy2) (intersection-interval y1 y2 y3 y4)
       (values (and ov1 ov2) ix1 iy1 ix2 iy2))))

(defun overlapping-area (x1 y1 x2 y2 x3 y3 x4 y4)
  "Compute the overlapping-area between the rectangle with diagonal
points (x1,y1) and (x2, y2) and the one with (x3, y3),(x4,y4)?
Assuming x1<x2, x3<x4, y same. Return 0 if they dont intersect at
all."
  (multiple-value-bind
	(ov ix1 iy1 ix2 iy2)
      (intersection-rectangle x1 y1 x2 y2 x3 y3 x4 y4)
    (cond
      (ov (* (- ix2 ix1) (- iy2 iy1))) (t 0))))

;; only supports 32 bit images

(defun load-file-to-sequence (file)
  (with-open-file (in file :element-type '(unsigned-byte 8)) 
	 (let* ((length (file-length in))
		(content (make-array (list length)
				     :element-type '(unsigned-byte 8)
				     :adjustable nil)))
	   (read-sequence content in) content)))

(defun write-file-from-sequence (file sequence)
  (with-open-file (out file :element-type '(unsigned-byte 8)
		       :direction :output)
    (write-sequence sequence out)))

(defun word-at (sequence elt)
  (+ (ash (elt sequence (1+ elt)) 8) (elt sequence elt)))

(defun set-word-at (sequence elt num)
  (setf (elt sequence elt) (mod num 256))
  (setf (elt sequence (1+ elt)) (mod (ash num -8) 256)))

(defun dword-at (sequence elt)
  (+
   (ash (elt sequence (+ 3 elt)) 24)
   (ash (elt sequence (+ 2 elt)) 16)
   (ash (elt sequence (+ 1 elt)) 8)
   (elt sequence elt)))

(defun set-dword-at (sequence elt num)
  (setf (elt sequence elt) (mod num 256))
  (setf (elt sequence (+ 1 elt)) (mod (ash num -8) 256))
  (setf (elt sequence (+ 2 elt)) (mod (ash num -16) 256))
  (setf (elt sequence (+ 3 elt)) (mod (ash num -24) 256)))

(defun signed-dword-at (sequence elt)
  (let ((r (dword-at sequence elt)))
    (if (> r (expt 2 31))
	(- r (expt 2 32))
	r)))

(defun set-signed-dword-at (sequence elt num)
  (set-dword-at sequence elt
		(if (> num 0) num
		    (+ num (expt 2 32)))))

(defun verify-bmp-magic-bytes (sequence)
  (= (word-at sequence 0) 19778))

(defun bmp-size-in-header (sequence)
  (dword-at sequence 2))

(defun bmp-image-data-offset (sequence)
  (dword-at sequence 10))

(defun bmp-bi-compression (sequence)
  (dword-at sequence 30))

(defun bmp-width (sequence)
  (signed-dword-at sequence 18))

(defun bmp-signed-height (sequence)
  (signed-dword-at sequence 22))

(defun bmp-height (sequence)
  (abs (bmp-signed-height sequence)))

(defun bmp-pixel-data (sequence &key (destructive nil))
  (let* ((w (bmp-width sequence))
	 (h (bmp-height sequence))
	 (o (bmp-image-data-offset sequence))
	 (l (* w h 4)))
    (if destructive
	(make-array (list l)
		    :element-type '(unsigned-byte 8)
		    :displaced-to sequence
		    :displaced-index-offset o)
	(subseq sequence o (+ o l)))))

(defun blit-image (x y src-width src-height src-blob
		   dst-width dst-height dst-blob)
  (declare (ignore dst-height))
  (do ((cx 0 (1+ cx))) ((= cx src-width))
    (do ((cy 0 (1+ cy))) ((= cy src-height))
      (let ((src-pos (* 4 (+ cx (* cy src-width))))
	    (dst-pos (* 4 (+ (+ x cx) (* (+ y cy) dst-width)))))
	(do ((i 0 (1+ i))) ((= i 4))
	  (setf (elt dst-blob (+ i dst-pos))
		(elt src-blob (+ i src-pos))))))))

(defun resize-pixeldata
    (argb-pixeldata old-width old-height new-width new-height
     &optional (new-pixeldata (make-array (list (* 4 new-width new-height))
					  :element-type '(unsigned-byte 8)
					  :adjustable nil)))
  (let*
      ((ccolor (make-array '(4)
			   :adjustable nil
			   :element-type 'rational))
       (times-x (/ old-width new-width))
       (times-y (/ old-height new-height)))
    (labels ((pixel-at (x y)
	       (let ((fpos (* 4 (+ x (* y old-width)))))
		 (make-array '(4)
			     :element-type '(unsigned-byte 8)
			     :displaced-to argb-pixeldata
			     :displaced-index-offset fpos)))
	     (new-pixel-at (x y)
	       (let ((fpos (* 4 (+ x (* y new-width)))))
		 (make-array '(4)
			     :element-type '(unsigned-byte 8)
			     :displaced-to new-pixeldata
			     :displaced-index-offset fpos)))
	     (color-of-rect (x1 y1 x2 y2 color-out)
	       (let*
		   ((area (* (- x2 x1) (- y2 y1))))
		 (dotimes (i 4) (setf (elt ccolor i) 0))
		 (loop for cy from (floor y1) to (ceiling y2) do
		      (loop for cx from (floor x1) to (ceiling x2) do
			   (let
			       ((c-area
				 (overlapping-area
				  x1 y1 x2 y2 cx cy
				  (1+ cx) (1+ cy))))
			     (map-into ccolor
				       #'(lambda (x y) 
					   (+ x (* c-area y)))
				       ccolor (pixel-at
					       (min cx (1- old-width))
					       (min cy (1- old-height)))))))
		 (map-into color-out
			   #'(lambda (x)
			       (round (/ x area))) ccolor)))
	     (interpol (x y color-out)
	       (color-of-rect (* times-x x)
			      (* times-y y)
			      (* times-x (1+ x))
			      (* times-y (1+ y))
			      color-out)))
      (do ((cy 0 (1+ cy))) ((= cy new-height))
	(do ((cx 0 (1+ cx))) ((= cx new-width))
	  (let ((np (new-pixel-at cx cy)))
		(interpol cx cy np))))
      new-pixeldata)))

(defun create-bmp-image (width height argb-data-get)
  "argb-data-get is a function taking an array on which it saves the
image-data (for efficiency-reasons)."
  (let*
      ((imagesize (* width height 4))
       (filesize (+ imagesize 54 333))
       (file-data (make-array (list filesize)
			      :element-type '(unsigned-byte 8)
			      :adjustable nil))
       (image-data (make-array (list imagesize)
			       :element-type '(unsigned-byte 8)
			       :displaced-to file-data
			       :displaced-index-offset 54)))
    ;; headings
    (set-word-at file-data 0 19778) ; magic number
    (set-dword-at file-data 2 filesize)
    (set-dword-at file-data 6 0) ; reserved
    (set-dword-at file-data 10 54) ; image-data-offset
    (set-dword-at file-data 14 40) ; header size
    (set-signed-dword-at file-data 18 width)
    (set-signed-dword-at file-data 22 height)
    (set-word-at file-data 26 1) ; not used here
    (set-word-at file-data 28 32) ; bits per pixel
    (set-dword-at file-data 30 0) ; image is in rgb-format
    (set-dword-at file-data 34 imagesize) ; size of image-data
    (set-signed-dword-at file-data 38 0) ; not used here
    (set-signed-dword-at file-data 42 0) ; not used here
    (set-dword-at file-data 46 0) ; no color table
    (set-dword-at file-data 50 0) ; no color table
    ;; data
    (funcall argb-data-get image-data)
    file-data))

(defun resize-bmp-blob (seq width height)
    (let*
      ((w (bmp-width seq))
       (h (bmp-height seq))
       (img (bmp-pixel-data seq))
       (res #'(lambda (seq) (resize-pixeldata img w h width height seq))))
      (create-bmp-image width height res)))


(defun resize-bmp-file (infile outfile width height)
  (write-file-from-sequence
   outfile
   (resize-bmp-blob (load-file-to-sequence infile) width height)))


(defun show-sdl-pixeldata (pixeldata width height)
  (labels ((pixel-at (x y)
		    (let ((fpos (* 4 (+ x (* y width)))))
		      ;(subseq pixeldata fpos (+ 4 fpos))
		      (make-array '(4)
				  :element-type '(unsigned-byte 8)
				  :displaced-to pixeldata
				  :displaced-index-offset fpos))))
    (sdl:with-init ()
      (sdl:window width height)
      (do ((cy 0 (1+ cy))) ((= cy height))
	(do ((cx 0 (1+ cx))) ((= cx width))
	  (let ((cpix (pixel-at cx cy)))
	    (sdl:draw-pixel-* (1+ cx) (1+ cy)
			      :color (sdl:color
				      :r (elt cpix 2)
				      :g (elt cpix 1)
				      :b (elt cpix 0))))))
      (sdl:update-display)
      (sdl:with-events ()
	(:idle  t)
	(:quit-event () t)))))

(defun show-sdl (filename)
  (let* ((seq (load-file-to-sequence filename))
	 (w (bmp-width seq))
	 (h (bmp-height seq))
	 (img (bmp-pixel-data seq)))
    (show-sdl-pixeldata img w h)))

(defun show-sdl-resized (filename width height)
  (let*
      ((seq (load-file-to-sequence filename))
       (w (bmp-width seq))
       (h (bmp-height seq))
       (img (bmp-pixel-data seq))
       (res (resize-pixeldata img w h width height)))
    (show-sdl-pixeldata res width height)))

(defun as-alpha-value (f b a)
  (coerce
   (round (/ (+ (* f a) (* b (- 255 a))) 255))
   '(unsigned-byte 8)))

(defun bmp-to-ppm (inblob background-rgb)
  (let*
      ((seq inblob)
       (width (bmp-width seq))
       (height (bmp-height seq))
       (img (bmp-pixel-data seq)))
    (with-output-to-string (out)
      (format out "P3~%")
      (format out "~d ~d~%255~%" width height)
      (labels ((pixel-at (x y)
		 (let ((fpos (* 4 (+ x (* y width)))))
		   (make-array '(4)
			       :element-type '(unsigned-byte 8)
			       :displaced-to img
			       :displaced-index-offset fpos))))
	(do ((cy 0 (1+ cy))) ((= cy height))
	  (do ((cx 0 (1+ cx))) ((= cx width))
	    (let* ((cpix (pixel-at cx cy))
		   (alpha (elt cpix 3))
		   (r (as-alpha-value
		       (elt cpix 2) (elt background-rgb 2) alpha))
		   (g (as-alpha-value
		       (elt cpix 1) (elt background-rgb 1) alpha))
		   (b (as-alpha-value
		       (elt cpix 0) (elt background-rgb 0) alpha))
		  )
	      (format out "~d ~d ~d~%" r g b))))))))
