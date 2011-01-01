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

(defun make-quad (id x y w h)
  (setf x (- (+ x x) +screen-width+ ))
  (setf y (- (+ y y)  +screen-height+))
  (gl:bind-texture :texture-2d id)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0) (gl:vertex x (+ y h))
    (gl:tex-coord 1 0) (gl:vertex  (+ x w) (+ y h))
    (gl:tex-coord 1 1) (gl:vertex (+ x w) y)
    (gl:tex-coord 0 1) (gl:vertex x y)))

