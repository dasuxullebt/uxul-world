;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(declaim (optimize (speed 3))
	 (inline rectangles-overlap is-horizontal is-vertical turn90
		 turn270))

(defun rectangles-overlap (x1 y1 x2 y2 x3 y3 x4 y4)
  "Does the rectangle with diagonal points (x1,y1) and (x2, y2)
overlap with (x3, y3),(x4,y4)? Assuming x1<x2, x3<x4, y same. We dont
add the limits to the rectangle, we only see the interior points."
  (and
    (> x2 x3)
    (> x4 x1)
    (> y2 y3)
    (> y4 y1)))
		

(defun symbol-prename (symbol &optional (charnum 1))
  "Returns just the first <charnum> Characters of the name of that symbol"
  (subseq (symbol-name symbol) 0 charnum))

(defun symbol-index (symbol &optional (charnum 1))
  "Removes the first (or charnum) character(s) of a Symbol and parses
the rest into an integer, i.e. makes 1 out of :R1"
  (parse-integer (subseq (symbol-name symbol) charnum)))

(defun is-horizontal (direction)
  (or (eq direction :LEFT) (eq direction :RIGHT)))

(defun is-vertical (direction)
  (or (eq direction :UP) (eq direction :DOWN)))

(defun turn90 (direction)
  (cond
    ((eq direction :LEFT) :UP)
    ((eq direction :RIGHT) :DOWN)
    ((eq direction :UP) :RIGHT)
    ((eq direction :DOWN) :LEFT)))

(defun turn270 (direction)
  (cond
    ((eq direction :LEFT) :DOWN)
    ((eq direction :RIGHT) :UP)
    ((eq direction :UP) :LEFT)
    ((eq direction :DOWN) :RIGHT)))

(defun string-ends-with (str1 str2)
  (let ((length1 (length str1))
	(length2 (length str2)))
    (and (>= length1 length2)
	 (string= (subseq str1 (- length1 length2)) str2))))

(defun lower-interval-bound (x1 x2 y1 y2)
  "Find the lower interval-bound of [x1, x2] /\ [y1, y2] or - if
disjoint - return NIL."
  (let ((xmin (min x1 x2))
	(xmax (max x1 x2))
	(ymin (min y1 y2))
	(ymax (max y1 y2)))
    (if (<= xmin ymin xmax) ymin
	(if (<= ymin xmin ymax) xmin NIL))))

(defmacro swapsort (a b)
  `(if (> ,a ,b)
       (rotatef ,a ,b)))

(defun move-collision-rectangle-about-x (moving-object delta-x)
  "this function is only a helper for a special case of the method
move-about for collision-objects, which is invoked iff there is no
movement in y-direction AND x is not zero"
  (let ((current-time 1)
	(current-collision NIL)
	(current-standing-object NIL))
    (dolist (standing-object (listen-to moving-object))
      (when (and (colliding standing-object) (not (eq standing-object moving-object)))
	(when (< (* 2 (abs (- (mid-y moving-object) (mid-y standing-object))))
		 (+ (height moving-object) (height standing-object)))
					;are the y-coordinates near enough such that a collision *can* occur?
	  (let* ((x-minimal-distance (+ (half-width moving-object) (half-width standing-object)))
		 (x-distance (- (mid-x standing-object) (mid-x moving-object)))
		 (x-collide-time-1
		  (/ (+ x-minimal-distance x-distance) delta-x))
		 (x-collide-time-2
		  (/ (- x-distance x-minimal-distance) delta-x))
		 (x-minimal-collide-time (min x-collide-time-1 x-collide-time-2)))
	    (when (<= 0 x-minimal-collide-time current-time)
					;an earlier collision can only
					;occur between 0 and the
					;current-time which is <1 and
					;maybe was set before.
	      (setf current-time x-minimal-collide-time)
	      (setf current-collision
		    (make-instance
		     'collision
		     :desired-movement (make-xy delta-x 0)
		     :pos (make-xy (+ (truncate (* current-time delta-x)) (x moving-object)) (y moving-object))
		     :collision-time current-time
		     :direction (if (> delta-x 0) :right :left)))
	      (setf current-standing-object standing-object))))))
      (if current-collision ;if a collision occured, this must be the first now
	  (on-collision moving-object current-standing-object current-collision)
	  (incf (x moving-object) delta-x)))
;;  (move-collision-rectangle-about-xy moving-object delta-x 0)
)


(defun move-collision-rectangle-about-y (moving-object delta-y)
  "this function is only a helper for a special case of the method
move-about for collision-objects, which is invoked iff there is no
movement in y-direction AND x is not zero"
  (let ((current-time 1)
	(current-collision NIL)
	(current-standing-object NIL))
    (dolist (standing-object (listen-to moving-object))
      (when (and (colliding standing-object) (not (eq standing-object moving-object)))
	(when (< (abs (* 2 (- (mid-x moving-object) (mid-x standing-object))))
		 (+ (width moving-object) (width standing-object)))
					;are the y-coordinates near enough such that a collision *can* occur?
	  (let* ((y-minimal-distance (+ (half-height moving-object) (half-height standing-object)))
		 (y-distance (- (mid-y standing-object) (mid-y moving-object)))
		 (y-collide-time-1
		  (/ (+ y-minimal-distance y-distance) delta-y))
		 (y-collide-time-2
		  (/ (- y-distance y-minimal-distance) delta-y))
		 (y-minimal-collide-time (min y-collide-time-1 y-collide-time-2)))
	    (when (<= 0 y-minimal-collide-time current-time)
					;an earlier collision can only occur between 0 and the current-time
					;which is <1 and maybe was set before.
	      (setf current-time y-minimal-collide-time)
	      (setf current-collision
		    (make-instance
		     'collision
		     :desired-movement (make-xy 0 delta-y)
		     :pos (make-xy (x moving-object) (+ (truncate (* current-time delta-y)) (y moving-object)))
		     :collision-time current-time
		     :direction (if (> delta-y 0) :down :up)))
	      (setf current-standing-object standing-object))))))
      (if current-collision ;if a collision occured, this must be the first now
	  (on-collision moving-object current-standing-object current-collision)
	  (incf (mid-y moving-object) delta-y)))
;;  (move-collision-rectangle-about-xy moving-object 0 delta-y)
)


;; Temporarily

(defun rational-= (n1 d1 n2 d2)
  (= (* n1 d2) (* n2 d1)))

#|(defun rational-< (n1 d1 n2 d2 &rest args)
  "Compare rationals even if the denominators are zero. Behaviour for
0/0 is not specified and may change."
  (and (if (zerop d1)
	   (if (zerop d2)
	       (< (signum n1) (signum n2))
	       (< n1 0))
	   (< (* n1 (abs d2) (signum d1)) (* n2 (abs d1) (signum d2))))
       (or (not args)
	   (apply #'rational-< n2 d2 args))))|#


#|(defun rational-<= (n1 d1 n2 d2 &rest args)
  "Compare rationals even if the denominators are zero. Behaviour for
0/0 is not specified and may change."
  (and (if (zerop d1)
	   (if (zerop d2)
	       (<= (signum n1) (signum n2))
	       (< n1 0))
	   (<= (* n1 (abs d2) (signum d1)) (* n2 (abs d1) (signum d2))))
       (or (not args)
	   (apply #'rational-<= n2 d2 args))))|#

(defun rational-<= (n1 d1 n2 d2 &rest args)
  (and
   (<= (* n1 (abs d2) (signum d1)) (* n2 (abs d1) (signum d2)))
   (or
    (not args)
    (apply #'rational-<= n2 d2 args))))

(defun rational-< (n1 d1 n2 d2 &rest args)
  (and
   (< (* n1 (abs d2) (signum d1)) (* n2 (abs d1) (signum d2)))
   (or
    (not args)
    (apply #'rational-< n2 d2 args))))

(defun rational-> (n1 d1 n2 d2 &rest args)
  (and (rational-< n2 d2 n1 d1)
       (or (not args)
	   (apply #'rational-> n2 d2 args))))

(defun rational->= (n1 d1 n2 d2 &rest args)
  (and (rational-<= n2 d2 n1 d1)
       (or (not args)
	   (apply #'rational->= n2 d2 args))))

#|FIIIIIIIIXMEEEEEEEEEEE!!!!!!!!!1111111!!!!!!!!!111

(defun move-collision-rectangle-about-xy (moving-object x y)
  "GIANT ugly but faster implementation than before..."
  (declare (type fixnum x y)
	   (type game-object moving-object))
  (let* ((current-time-num 1)
	 (current-time-denom 1)
	 (collision-with-x nil)
	 (collision-with-y nil)
	 (current-standing-object nil)
	 (wm (width moving-object))
	 (hm (height moving-object))
	 (x1 (x moving-object))
	 (y1 (y moving-object))
	 (x2 (+ x1 wm))
	 (y2 (+ y1 hm)))
    (declare (type fixnum current-time-num current-time-denom wm x1
			   y1 x2 y2)
	     (type boolean collision-with-x collision-with-y))
    (dolist (standing-object (listen-to moving-object))
      (let* ((ws (width standing-object))
	     (hs (height standing-object))
	     (x3 (x standing-object))
	     (y3 (y standing-object))
	     (x4 (+ x3 ws))
	     (y4 (+ y3 hs))
	     (y-enter (- (sgn y))) ;; gain negative infty for y-enter/0
	     (y-leave y)
	     (x-enter (- (sgn x))) ;; gain negative infty for x-enter/0
	     (x-leave x))
	(declare (type fixnum ws hs x3 y3 x4 y4 y-enter y-leave
		       x-enter x-leave))
	(and
	 ;; is the object colliding? does the standing object overlap a (huge enough)
	 ;; rectangle around the moving object?
	 
	 (colliding standing-object)
#|	 (rectangles-overlap (- x1 absx)
			     (- y1 absy)
			     (+ x2 absx 1)
			     (+ y2 absy 1)
			     x3 y3 (1+ x4) (1+ y4))|#
	 (cond
	   ((> x2 x3)
	    (cond
	      ((> x4 x1)
	       ;;x-enter = -1
;;	       (format t "x-inside")
	       (macrolet ((calc-x ()
			    `(progn
			       (if (> x 0)
				   (setf x-leave (- x4 x1))
				   (setf x-leave (- x3 x2))))))	       
		 (cond
		   ((> y2 y3)
		    (cond
		      ((> y4 y1)
		       ;; rectangles do overlap before movement ... do
		       ;; nothing.
		       nil)
		      (T ; y4 <= y1
		       ;; standing-object is over moving-object
		       (cond
			 ((>= y 0)
			  ;; no collision - wrong direction, or no
			  ;; movement at all.
			  nil)
			 (T
			  ;; collision may occur
			  (setf y-enter (- y1 y4))
			  (setf y-leave (- y3 y2))
			  (calc-x)
			  T)))))
		   (T ; y2 <= y3
		    ;; standing-object is below moving-object
		    (cond
		      ((<= y 0)		     
		       ;; no collision - wrong direction, or no movement
		       ;; at all.
		       nil)
		      (T
		       ;; collision may occur
		       (setf y-enter (- y3 y2))
		       (setf y-leave (- y4 y1))
		       (calc-x)
		       T))))))
	      (T ; x4 <= x1
	       ;; standing-rectangle left of moving-rectangle
	       (macrolet ((calc-x ()
			    ;; x will be <= 0
			    `(progn (setf x-enter (- x3 x2))
				    (setf x-leave (- x4 x1)))))
		 (cond
		   ((> x 0)
		    ;; no collision - wrong direction, or no movement at
		    ;; all.
		    nil)
		   (T
		    ;; collision may occur - check y
		    (cond
		      ((> y2 y3)
		       (cond
			 ((> y4 y1)
			  ;; y-enter = 0
			  (setf y-leave (if (> y 0) (- y2 y3) (- y1 y4)))
			  (calc-x)
			  T)
			 (T ; y4 <= y1
			  ;; standing-object is over moving-object
			  (cond
			    ((>= y 0)
			     ;; no collision - wrong direction, or no
			     ;; movement at all.
			     nil)
			    (T
			     ;; collision may occur
			     (setf y-enter (- y4 y1))
			     (setf y-leave (- y3 y2))
			     (calc-x)
			     T)))))
		      (T ; y2 < y3
		       ;; standing-object is below moving-object
		       (cond
			 ((<= y 0)		     
			  ;; no collision - wrong direction, or no movement
			  ;; at all.
			  nil)
			 (T
			  ;; collision may occur
			  (setf y-enter (- y3 y2))
			  (setf y-leave (- y4 y1))
			  (calc-x)
			  T))))))))))
	   (T ; x2 <= x3
	    ;; standing-rectangle right of moving-rectangle
	    (macrolet ((calc-x ()
			 ;; will be x > 0
			 '(progn
			   (setf x-leave (- x2 x3))
			   (setf x-enter (- x1 x4)))))
	    (cond
	      ((<= x 0)
	       ;; no collision - wrong direction, or no movement at
	       ;; all.
	       nil)
	      (T
	       ;; collision may occur - check y
	       (cond
		 ((> y2 y3)
		  (cond
		    ((> y4 y1)
		     ;; y-bounds of standing-object lie completely
		     ;; inside y-bounds of moving-object.
		     (setf y-leave (if (> y 0) (- y1 y4) (- y2 y3)))
		     (calc-x)
		     T)
		    (T ; y4 < y1
		     ;; standing-object is over moving-object
		     (cond
			  ((>= y 0)
			   ;; no collision - wrong direction, or no
			   ;; movement at all.
			   nil)
			  (T
			   ;; collision may occur
			   (setf y-enter (- y1 y4))
			   (setf y-leave (- y3 y2))
			   (calc-x)
			   T)))))
		 (T ; y2 < y3
		  ;; standing-object is below moving-object
		  (cond
		    ((<= y 0)		     
		     ;; no collision - wrong direction, or no movement
		     ;; at all.
		     nil)
		    (T
		     ;; collision may occur
		     (setf y-enter (- y3 y2))
		     (setf y-leave (- y1 y4))
		     (calc-x)
		     T)))))))))

	 ;; collision could occure - find the smallest collision-time

	 (progn (format t "---~%could occure.~%current ~d/~d~%x-enter ~d/~d~%x-leave ~d/~d~%y-enter ~d/~d~%y-leave ~d/~d~%---~%"
			current-time-num current-time-denom
			x-enter x x-leave x y-enter y y-leave y) t)
	 
	 (cond
	   ((rational-<= x-enter x y-enter y x-leave x)
	    ;; first collision-time is y-enter/y - check if this is
	    ;; smaller (earlier) than current-time-num/current-time-denom
	    ;; and later than 0	      
	    (when (rational-< y-enter y current-time-num current-time-denom)
	      (setf current-standing-object standing-object)
	      (setf current-time-denom y)
	      (setf current-time-num y-enter)
	      (setf collision-with-y t)
	      (setf collision-with-x (rational-<= y-enter y y-enter x y-leave y))))
	   ((rational-<= y-enter y x-enter x y-leave y) ;; first collision-time is x-enter/y
	    (when (rational-< x-enter x current-time-num current-time-denom)
	      (setf current-standing-object standing-object)
	      (setf current-time-denom x)
	      (setf current-time-num x-enter)
	      (setf collision-with-x t)
	      (setf collision-with-y nil)))
	   (T nil)))))
    (cond
      (current-standing-object
       (format t "occured~d~%" current-time-num)
;       (write (cons current-time-num current-time-denom))
       ;; a collision occured
       (on-collision moving-object current-standing-object
		     (make-instance 'collision
				    :desired-movement (make-xy x y)
				    :collision-time
				    (the rational (/ current-time-num
						     current-time-denom))
				    :pos (make-xy
					  (+ (truncate (* current-time-num x) current-time-denom) x1)
					  (+ (truncate (* current-time-num y) current-time-denom) y1))
				    :direction (if collision-with-x
						   (if collision-with-y
						       :diagonal
						       (if (> y 0) :down :up))
						   (if (> x 0) :right :left)))))
      (T
       (setf (x moving-object) (+ x1 x))
       (setf (y moving-object) (+ y1 y))))))|#




(defun move-collision-rectangle-about-xy (moving-object x y)
  "this function is only a helper for a special case of the method
move-about for collision-objects, which is invoked iff both x and y
are not zero"
  (declare (optimize (debug 0) (safety 0) (space 0) (compilation-speed 0) (speed 3))
	   (type fixnum x y)
	   (type game-object moving-object))
  (let ((absx (abs x))
	(absy (abs y))
	      (xm (x moving-object))
	      (ym (y moving-object))
	      (wm (width moving-object))
	      (hm (height moving-object))
	      (2*current-time-num 2)
	      (current-time-denom 1)
	      (current-standing-object NIL)
	      (current-direction nil))
	  (declare (type fixnum xm ym wm hm 2*current-time-num
			 current-time-denom absx absy)
		   (type symbol current-direction)
		   )
	  (let ((2*mid-x-moving (the fixnum (+ xm xm wm)))
		(2*mid-y-moving (the fixnum (+ ym ym hm))))
	    (declare (type fixnum 2*mid-x-moving 2*mid-y-moving))
	    (dolist (standing-object (listen-to moving-object))
	      (when (and
		     
	     ;;;;;;;;;;;;; BEEEEEEEEEEETTTTTTTTTTEEEEEEEEEEEEEEEEEERRRRRRRRRRRRRRRR!!!!!!!!!!!!!!!!!!
		     
		     (colliding standing-object)

#|		     (rectangles-overlap (- xm absx) (- ym absy) (+ xm wm absy) (+ ym hm absx)
					 (- xs absx) (- ys absy) (+ xs ws absx) (+ ys hs absy))

		     (not
		      (rectangles-overlap xm ym (+ xm wm) (+ ym hm)
					  xs ys (+ xs hs) (+ ys hs)))|#

		     (not (eq moving-object standing-object))
		     
		     )
		(let* ((xs (x standing-object))
		       (ys (y standing-object))
		       (ws (width standing-object))
		       (hs (height standing-object))
		       (temporary-direction nil)
		       (2*x-minimal-distance (the fixnum (+ wm ws)))
		       (2*y-minimal-distance (the fixnum (+ hm hs)))
		       (2*x-distance (the fixnum (- (+ xs xs ws) 2*mid-x-moving)))
		       (2*y-distance (the fixnum (- (+ ys ys hs) 2*mid-y-moving)))
		       (2*x-collide-time-1 (the fixnum (+ 2*x-minimal-distance 2*x-distance)))
		       (2*x-collide-time-2 (the fixnum (- 2*x-distance 2*x-minimal-distance)))
		       (2*y-collide-time-1 (the fixnum (+ 2*y-minimal-distance 2*y-distance)))
		       (2*y-collide-time-2 (the fixnum (- 2*y-distance 2*y-minimal-distance)))
		       (minimal-collide-time-denom 0)
		       (2*minimal-collide-time-num
			(progn
			  (if (> x 0)
			      (if (> 2*x-collide-time-1 2*x-collide-time-2)
				  (rotatef 2*x-collide-time-1 2*x-collide-time-2))
			      (if (< 2*x-collide-time-1 2*x-collide-time-2)
				  (rotatef 2*x-collide-time-1 2*x-collide-time-2)))
			  (if (> y 0)
			      (if (> 2*y-collide-time-1 2*y-collide-time-2)
				  (rotatef 2*y-collide-time-1 2*y-collide-time-2))
			      (if (< 2*y-collide-time-1 2*y-collide-time-2)
				  (rotatef 2*y-collide-time-1 2*y-collide-time-2)))
			  (cond
			    ((rational-<= 2*x-collide-time-1 x 2*y-collide-time-1 y 2*x-collide-time-2 x)
			     (setf minimal-collide-time-denom y)
			     (setf temporary-direction (if (> y 0) :down :up))
			     2*y-collide-time-1)
			    ((rational-<= 2*y-collide-time-1 y 2*x-collide-time-1 x 2*y-collide-time-2 y)
			     (setf minimal-collide-time-denom x)
			     (setf temporary-direction (if (> x 0) :right :left))
			     2*x-collide-time-1)
			    (T 0)))))
		  (declare (type fixnum xs ys ws hs 2*x-minimal-distance
				 2*y-minimal-distance 2*x-distance
				 2*y-distance 2*x-collide-time-1
				 2*x-collide-time-2 2*y-collide-time-1
				 2*y-collide-time-2
				 2*minimal-collide-time-num
				 minimal-collide-time-denom)
			   (type symbol temporary-direction))
		  (when (and (not (zerop minimal-collide-time-denom))
			     (rational-<= 0 1
					  2*minimal-collide-time-num minimal-collide-time-denom
					  2*current-time-num current-time-denom))
		    (setf 2*current-time-num 2*minimal-collide-time-num)
		    (setf current-time-denom minimal-collide-time-denom)
		    (setf current-direction
			  (cond
			    ((or (eq temporary-direction :right)
				 (eq temporary-direction :left))
			     (if (or (rational-= 2*minimal-collide-time-num
						 minimal-collide-time-denom
						 2*y-collide-time-1
						 y)
				     (rational-= 2*minimal-collide-time-num
						 minimal-collide-time-denom
						 2*y-collide-time-2
						 y))
				 :diagonal
				 temporary-direction))
			    ((or (eq temporary-direction :up)
				 (eq temporary-direction :down))
			     (if (or (rational-= 2*minimal-collide-time-num
						 minimal-collide-time-denom
						 2*x-collide-time-1
						 x)
				     (rational-= 2*minimal-collide-time-num
						 minimal-collide-time-denom
						 2*x-collide-time-2
						 x))
				 :diagonal
				 temporary-direction))))
		    (setf current-standing-object standing-object)))))
	    (if current-direction
		(on-collision moving-object current-standing-object
			      (make-instance 'collision
					     :desired-movement (make-xy x y)
					     :collision-time (the rational (/ 2*current-time-num (* 2 current-time-denom)))
					     :pos (make-xy
						   (+
						    (truncate (the fixnum (* 2*current-time-num x))
							      (the fixnum (* 2 current-time-denom)))
						    (x moving-object))
						   (+ (truncate (the fixnum (* 2*current-time-num y))
								(the fixnum (* 2 current-time-denom)))
						      (y moving-object)))
					     :direction current-direction))			
		(progn
		  (setf (x moving-object) (the fixnum (+ (x moving-object) x)))
		  (setf (y moving-object) (the fixnum (+ (y moving-object) y)))
		  )))))
  

(defun old-draw-rectangle (obj &key (r 0) (g 0) (b 0))
  (declare (type game-object obj))
  (sdl:draw-rectangle-* (zoom-trans (+ *current-translation-x* (x obj)))
			(zoom-trans (+ *current-translation-y* (y obj)))
			(zoom-trans (width obj))
			(zoom-trans (height obj))
			:color (sdl:color :r r :g g :b b)))
