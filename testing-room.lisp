;;; Copyright 2009-2011 Christoph Senjak

(in-package :uxul-world)


#|
(defmethod (setf x) (new-value (obj T)))
(defmethod (setf y) (new-value (obj T)))
(defmethod (setf visible) (new-value (obj T)))
|#

(defun make-testing-room ()
  "Create a simple room for testing. Shouldnt be used anymore. Use the
level-editor instead!"
  (let* ((player (make-instance 'player
				:active t
				:visible t
				:redraw t
				:x 100
				:y 0))
	 (ret (make-instance 'room
			     :width 0;(* 155 128)
			     :height 0;(* 9 128)
			     :key-listener player
			     :graphic-centralizer player
			     :key-up-function
			     #'(lambda (key) (on-key-up player key))
			     :key-down-function
			     #'(lambda (key) (on-key-down player key))
			     )))
    (add-object player ret)

    (add-object (make-instance 'burning-marshmallow
			       :x (* 128 55)
			       :y (* 128 8)
			       :inner-rectangle (list (* 40 128) (* 5 128) (* 65 128) (* 9 128))
			       :active t
			       :visible t
			       :redraw t) ret)

;;***************************
    (add-object (make-instance 'key
			       :x (* 128 56)
			       :y (* 128 8)
			       :dungeon :testing-room
			       :visible t
			       :redraw t) ret)


    (add-object (make-instance 'burning-marshmallow
			       :x (* 128 60)
			       :y (* 128 8)
			       :inner-rectangle (list (* 40 128) (* 5 128) (* 65 128) (* 9 128))
			       :active t
			       :visible t
			       :redraw t) ret)

    (add-object (make-instance 'burning-marshmallow
			       :x (* 128 45)
			       :y (* 128 8)
			       :inner-rectangle (list (* 40 128) (* 5 128) (* 65 128) (* 9 128))
			       :active t
			       :visible t
			       :redraw t) ret)

    (add-object (make-instance 'burning-marshmallow
			       :x (* 128 34)
			       :y (* 128 4)
			       :inner-rectangle (list (* 30 128) (* 5 128) (* 41 128) (* 9 128))
			       :active t
			       :visible t
			       :redraw t) ret)

    (add-object (make-instance 'simple-enemy
				 :y (* 128 8)
				 :x (* 128 4)
				 :redraw t
				 :active t
				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 8)
				 :x (* 128 9)
				 :redraw t
				 :active t
				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 8)
				 :x (* 128 15)
				 :redraw t
				 :active t
 				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 3)
				 :x (* 128 16)
				 :redraw t
				 :active t
				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 7)
				 :x (* 128 20)
				 :redraw t
				 :active t
				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 6)
				 :x (* 128 21)
				 :redraw t
				 :active t
				 :visible t) ret)
    (add-object (make-instance 'simple-enemy
				 :y (* 128 8)
				 :x (* 128 34)
				 :redraw t
				 :active t
				 :visible t) ret)

    (dotimes (i 155)
      (add-object
       (make-instance 'stone
		      :y (* 128 9)
		      :x (* 128 i)
		      :active nil
		      :visible t
		      :redraw t) ret))

    (add-object
     (make-instance 'stone
		    :y (* 128 4)
		    :x (* 128 14)
		    :active nil
		    :visible t
		    :redraw t
) ret)
    (add-object
     (make-instance 'stone
		    :y (* 128 4)
		    :x (* 128 15)
		    :active nil
		    :visible t
		    :redraw t
) ret)

    (dotimes (i 7)
      (add-object (make-instance 'stone
				 :x (* 17 128)
				 :y (* i 128)
				 :active nil
				 :visible t
				 :redraw t
) ret))
    (dotimes (i 4)
      (add-object (make-instance 'leaf
				 :x (* (+ 18 i) 128)
				 :y (* 7 128)) ret))
    (dotimes (i 4)
      (add-object (make-instance 'leaf
				 :x (* (+ 19 i) 128)
				 :y (* 6 128)) ret))

    (add-object (make-instance 'leaf
				 :x (* 21 128)
				 :y (* 4 128)) ret)

    (dotimes (i 4)
      (dotimes (j 6)
	(add-object (make-instance 'stone
				   :x (* (+ 23 i) 128)
				   :y (* (+ 3 j) 128)
				   :active nil
				   :visible t
				   :redraw t) ret)))

    (add-object (make-instance 'stone
				   :x (* 37 128)
				   :y (* 8 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 39 128)
				   :y (* 8 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 39 128)
				   :y (* 7 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 40 128)
				   :y (* 8 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 41 128)
				   :y (* 8 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 41 128)
				   :y (* 7 128)
				   :active nil
				   :visible t
				   :redraw t) ret)
    (add-object (make-instance 'stone
				   :x (* 41 128)
				   :y (* 6 128)
				   :active nil
				   :visible t
				   :redraw t) ret)

    (dotimes (i 16)
      (add-object (make-instance 'stone
				 :x (* (+ i 44) 128)
				 :y (* 4 128)
				 :active nil
				 :visible t
				 :redraw t
) ret))

    (dotimes (i 5)
      (dotimes (j (1+ i))
	(add-object (make-instance 'stone
				 :x (* (+ i 65) 128)
				 :y (* (+ (- 4 i) j 4) 128)
				 :active nil
				 :visible t
				 :redraw t
) ret)))

    (dotimes (i 3)
      (dotimes (j 3)
	(add-object (make-instance 'stone
				 :x (* (+ i 70) 128)
				 :y (* (+ j 3) 128)
				 :active nil
				 :visible t
				 :redraw t
) ret)))

    (dotimes (j 2)
      (dotimes (i 8)
	(add-object (make-instance 'stone
				 :x (* (+ i j 72) 128)
				 :y (* (- 8 i) 128)
				 :active nil
				 :visible t
				 :redraw t
) ret)))
    (dotimes (j 7)
      (dolist (i (cond
		   ((member j '(0 1 2)) '(83))
		   ((member j '(3 4 5)) '(83 84 85 86))
		   (T '(79 80 81 82 83 84 85 86))))
	(add-object (make-instance 'stone
				   :x (* i 128)
				   :y (* j 128)
				   :active nil
				   :visible t
				   :redraw t
) ret)))

    (let ((y (* 128 4)))
      (dolist (j '((0 0 0 0 0 1 1 1 2)
		   (0 0 0 1 1 1 0 0 2)
		   (0 0 0 0 1 1 1 1 2)
		   (1 1 1 1 1 0 0 0 2)
		   (0 0 0 1 1 1 1 1 2)))
	(let ((x (* 128 87)))
	  (dolist (i j)
	    (cond ((eql i 2)
		   (add-object (make-instance 'stone
					      :x x
					      :y y
					      :active nil
					      :visible t
					      :redraw t
) ret))
		  ((eql i 1)
		 (add-object (make-instance 'leaf
					    :x x
					    :y y) ret))
		  (T))
	    (incf x 128)))
	(incf y 128)))
    (add-object (make-instance 'flying-nasobem
			       :x (* 128 87)
			       :y (* 128 2)) ret)
    ;;*******************************
    (add-object (make-instance 'door
			       :x (* 128 87)
			       :y (* 128 1)
			       :dungeon :testing-room
			       :visible t
			       :redraw t) ret)
    (add-object (make-instance 'door
			       :x (* 128 89)
			       :y (* 128 1)
			       :dungeon :testing-room
			       :visible t
			       :redraw t) ret)

    (add-object (make-instance 'flying-nasobem
			       :x (* 128 110)
			       :y (* 128 4)) ret)
    ret))
