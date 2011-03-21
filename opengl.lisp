;; Copyright 2010-2011 Christoph Senjak

(in-package :uxul-world)

(defun load-bmp-blob-into-texture (blob)
  (let*
      ((id (car (gl:gen-textures 1)))
       (pix (bmp-pixel-data blob))
       (w (bmp-width blob))
       (h (bmp-height blob)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 :rgba8 w h 0 :bgra :unsigned-byte pix)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:flush)
    id))

(defun load-spritesheet ()
  (let*
      ((id (car (gl:gen-textures 1)))
       (wh (cadr *spritesheet*))
       (pix (car *spritesheet*)))
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 :rgba8 wh wh 0 :bgra :unsigned-byte pix)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:flush)
    id))

(defun make-quad (imgs x y w h)
  (destructuring-bind (x1 y1 x2 y2 bla blubb) imgs
    (declare (ignore bla blubb))
    (setf x (- x +screen-width+))
    (setf y (- y +screen-height+))
    ;(gl:bind-texture :texture-2d *spritesheet-id*)
    (macrolet ((writedown (&rest vars)
		 `(progn
		    ,@(mapcar #'(lambda (var)
				 `(setf (cffi:mem-aref uxul-world::*ptr* :float (1- (incf uxul-world::*offset*))) (float ,var 0.0))) vars))))
      (writedown x1 y1 x (+ y h)
		 x2 y1 (+ x w) (+ y h)
		 x2 y2 (+ x w) y
		 x1 y2 x y))))