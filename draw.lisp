;;; Copyright 2009 Christoph Senjak

;; various draw-methods

(in-package :uxul-world)

(defun draw-background (x-trans y-trans)


  (flet ((modf (x y)
	     (if nil ;(< x 0)
		 (- y (mod x y))
		 (mod x y))))


  #|(sdl:draw-rectangle-* (+ 100 (ceiling (/ (mod x-trans 800) 2))) (+ 100 (ceiling (/ (mod y-trans 800) 2))) 400 400
			:color (sdl:color :r 255 :g 255 :b 255))|#
  ;; layer -1

    ;; HAAAAAAAAAAAAACK
    (incf x-trans (- 10000 300))
    (incf x-trans (- 10000 300))

  (dolist (i '(8 9 10 11 12 13 14 15 16 17 18 19 20))
    (dolist (j '(8 9 10 11 12 13 14 15 16 17 18 19 20))
      (sdl:draw-box-* (+ (- +screen-width+) (* 100 i) (round (/ (modf x-trans (* 2 +screen-width+)) 16))) (+ (- +screen-width+) (* 100 j) (ceiling (/ (modf y-trans (* 2 +screen-height+)) 16))) 50 50
			    :color (sdl:color :r (+ 128 64) :g (+ 128 64) :b (+ 128 64)))) :fill t)

  (dolist (i '(4 5 6 7 8 9))
    (dolist (j '(4 5 6 7 8 9))
      (sdl:draw-box-* (+ (- +screen-width+) (* 200 i) (round (/ (modf x-trans (* 2 +screen-width+)) 8))) (+ (- +screen-width+) (* 200 j) (ceiling (/ (modf y-trans (* 2 +screen-height+)) 8))) 100 100
			    :color (sdl:color :r 128 :g 128 :b 128))) :fill t)
  (dolist (i '(2 3 4 5))
    (dolist (j '(2 3 4 5))
      (sdl:draw-box-* (+ (- +screen-width+) (* 400 i) (round (/ (modf x-trans (* 2 +screen-width+)) 4))) (+ (- +screen-width+) (* 400 j) (ceiling (/ (modf y-trans (* 2 +screen-height+)) 4))) 200 200
			    :color (sdl:color :r 64 :g 64 :b 64))) :fill t)
  (dotimes (i 4)
    (dotimes (j 4)
      (sdl:draw-box-* (+ (- +screen-width+) (* 800 i) (round (/ (modf x-trans (* 2 +screen-width+)) 2))) (+ (- +screen-width+) (* 800 j) (ceiling (/ (modf y-trans (* 2 +screen-height+)) 2))) 400 400
			    :color (sdl:color :r 0 :g 0 :b 0))) :fill t)))




(defmethod draw ((obj room))
  (let ((*current-translation-x*
	 #|(cond
	   ((< (- (x (graphic-centralizer obj)) 400) 0) 0)
	   ((> (+ (x (graphic-centralizer obj)) 400) (width obj))
	    (- 800 (width obj)))
	   (T
	    (- 400 (x (graphic-centralizer obj)))))|#
	 (- 400 (x (graphic-centralizer obj)))
	  )
	(*current-translation-y*
	 #|(cond
	   ((< (- (y (graphic-centralizer obj)) 300) 0) 0)
	   ((> (+ (y (graphic-centralizer obj)) 300) (height obj))
	    (- 600 (height obj)))
	   (T
	    (- 300 (y (graphic-centralizer obj)))))|#
	 (- 300 (y (graphic-centralizer obj)))
	  ))
    ;;(draw-background *current-translation-x* *current-translation-y*)
    (dolist (image (get-objects obj 'uxul-world::game-object))
      (if (and (redraw image) (visible image)) (draw image)))))


;; FIXME

(defvar *player-bar-color* -255)

(defmethod draw ((obj player))
  #+nil(if (rectangle-in-screen obj)
	   (old-draw-rectangle obj :r 255 :g 255 :b 255))

  ;;; FIXME ************
  (sdl:draw-box-*
   10 10 (floor (* (power obj) (/ (- +screen-width+ 20) 10))) 10
   :color (sdl:color :r (abs *player-bar-color*) :g (abs *player-bar-color*) :b (abs *player-bar-color*)))
  (incf *player-bar-color* 5)
  (if (= *player-bar-color* 255) (setf *player-bar-color* -255))

  (call-next-method))

(defmethod draw ((obj stone))
  (call-next-method)
  #+nil(if (rectangle-in-screen obj)
      (old-draw-rectangle obj :r 255 :g 255 :b 255)))

(defmethod draw ((obj simple-enemy))
  (call-next-method)
  #+nil(if (rectangle-in-screen obj)
      (old-draw-rectangle obj :r 255 :g 255 :b 255)))