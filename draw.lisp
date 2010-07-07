;;; Copyright 2009 Christoph Senjak

;; various draw-methods

(in-package :uxul-world)

(defun draw-background (x-trans y-trans)
  ;; (let ((ani3 (car (images (make-animation 0 |background_test_layer_3|))))
;; 	(ani2 (car (images (make-animation 0 |background_test_layer_2|)))))

;;     (loop for i from -1 to 16
;; 	 do (loop for j from -1 to 12
;; 		 do (progn
;; 		      (sdl:draw-surface-at-* ani2
;; 					     (+ (* i 64) (round
;; 							  (mod (/ x-trans 4) 64)))
;; 					     (+ (* j 64) (round
;; 							  (mod (/ y-trans 4) 64)))))))
;;     (loop for i from -1 to 16
;; 	 do (loop for j from -1 to 12
;; 		 do 
;; 		 (sdl:draw-surface-at-* ani3
;; 					     (+ (* 64 i) (round
;; 							  (mod (/ x-trans 2) 64)))
;; 					     (+ (* 64 j) (round
;; 							  (mod (/ y-trans 2) 64)))))))
  )

(defmethod draw ((obj room))
  (let ((*current-translation-x*
	 #|(cond
	   ((< (- (x (graphic-centralizer obj)) 400) 0) 0)
	   ((> (+ (x (graphic-centralizer obj)) 400) (width obj))
	    (- 800 (width obj)))
	   (T
	    (- 400 (x (graphic-centralizer obj)))))|#
	 (- (ash 400 (- *zoom-ash*)) (x (graphic-centralizer obj)))
	  )
	(*current-translation-y*
	 #|(cond
	   ((< (- (y (graphic-centralizer obj)) 300) 0) 0)
	   ((> (+ (y (graphic-centralizer obj)) 300) (height obj))
	    (- 600 (height obj)))
	   (T
	    (- 300 (y (graphic-centralizer obj)))))|#
	 (- (ash 300 (- *zoom-ash*)) (y (graphic-centralizer obj)))
	  ))
    (draw-background *current-translation-x* *current-translation-y*)
    (dolist (image (get-objects obj 'uxul-world::game-object))
      (if (and (redraw image)
	       (visible image)
	       (rectangle-in-screen image)) (draw image)))))


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
