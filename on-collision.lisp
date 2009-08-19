;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defmethod on-collision ((obj T) (obj2 T) collision)
  "Per default do not react on objects at all. Warn only."
  (format t "Warning: On-Collision is not overridden for some object
  it is called for. Classes of Arguments: ~A ~A~%"
	  (class-name (class-of obj))
	  (class-name (class-of obj))))

;; Player colliding with other objects

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle stone)
     (collision collision))
  (if (eql (direction collision) :DOWN)
      ;; "bottom" - allow jumping again
      (setf (mayjump moving-rectangle) T)
      ;; "ceiling" - dont allow continuing jump
      (if (eql (direction collision) :UP)
	  (setf (maycontjump moving-rectangle) nil))
      )
  (collide-blocks moving-rectangle standing-rectangle collision))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle tulip)
     (collision collision))
  (setf (visible standing-rectangle) nil)
  (setf (active standing-rectangle) nil)
  (setf (colliding standing-rectangle) nil)
  (if (< (power moving-rectangle) 10)
      (incf (power moving-rectangle)))
  (incf (tulips moving-rectangle)))
 
(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle bottom)
     (collision collision))
  (if (eql (direction collision) :DOWN)
      (call-next-method)
      ;; else
      (progn
	(setf (colliding standing-rectangle) nil)
	(move-about moving-rectangle (desired-movement collision))
	(setf (colliding standing-rectangle) t))))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle moving-enemy)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (setf (bounced moving-rectangle) T)
  (setf (mayjump moving-rectangle) T)
  (setf (autojump moving-rectangle) 5)
  (player-hits-enemy moving-rectangle
		     standing-rectangle
		     collision))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle standing-enemy)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (player-hits-enemy moving-rectangle
		     standing-rectangle
		     collision))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle standing-item)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (item-catch standing-rectangle moving-rectangle))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle moving-item)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (item-catch standing-rectangle moving-rectangle))


;; moving-item colliding with other objects

(defmethod on-collision
    ((moving-rectangle moving-item)
     (standing-rectangle player)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (item-catch moving-rectangle standing-rectangle))

(defmethod on-collision
    ((moving-rectangle moving-item)
     (standing-rectangle bottom)
     (collision collision))
  (if (eql (direction collision) :DOWN)
      (call-next-method)
      ;; else
      (progn
	(setf (colliding standing-rectangle) nil)
	(move-about moving-rectangle (desired-movement collision))
	(setf (colliding standing-rectangle) t))))

(defmethod on-collision
    ((moving-rectangle moving-item)
     (standing-rectangle stone)
     (collision collision))
  (collide-blocks moving-rectangle standing-rectangle collision))

;; simple-enemy special methods

(defmethod on-collision ((m simple-enemy) (s stone) (c collision))
  (cond ((eql (direction c) :left)
	 (setf (direction m) :right))
	((eql (direction c) :right)
	 (setf (direction m) :left)))
  (collide-blocks m s c))

;; burning-marshmallow special methods

(defmethod on-collision ((m burning-marshmallow) (s stone) (c collision))
  (cond
    ((eql (direction c) :LEFT)
     (setf (horizontal-direction m) :RIGHT))
    ((eql (direction c) :RIGHT)
     (setf (horizontal-direction m) :LEFT))
    ((eql (direction c) :UP)
     (setf (vertical-direction m) :DOWN))
    ((eql (direction c) :DOWN)
     (setf (vertical-direction m) :UP))
    (T ;; diagonal
     (setf (horizontal-direction m)
	   (if (eql (horizontal-direction m) :LEFT) :RIGHT :LEFT))
     (setf (vertical-direction m)
	   (if (eql (vertical-direction m) :UP) :DOWN :UP))))
  (set-burning-marshmallow-animation m))

(defmethod on-collision
    ((moving-rectangle burning-marshmallow)
     (standing-rectangle bottom)
     (collision collision))
  (if (eql (direction collision) :DOWN)
      (call-next-method)
      ;; else
      (progn
	(setf (colliding standing-rectangle) nil)
	(move-about moving-rectangle (desired-movement collision))
	(setf (colliding standing-rectangle) t))))

(defmethod on-collision
    ((moving-rectangle burning-marshmallow)
     (standing-rectangle player)
     (collision collision))
  (enemy-hits-player moving-rectangle
		     standing-rectangle
		     collision)
  (setf (colliding standing-rectangle) nil)
  (move-about moving-rectangle (desired-movement collision))
  (setf (colliding standing-rectangle) t))

(defmethod on-collision
    ((moving-rectangle player)
     (standing-rectangle burning-marshmallow)
     (collision collision))
  (enemy-hits-player standing-rectangle
		     moving-rectangle
		     collision)
  (setf (colliding standing-rectangle) nil)
  (move-about moving-rectangle (desired-movement collision))
  (setf (colliding standing-rectangle) t))
;; moving-enemy colliding with other objects

(defmethod on-collision ((m moving-enemy) (s stone) (c collision))
  (collide-blocks m s c))

(defmethod on-collision
    ((moving-rectangle moving-enemy)
     (standing-rectangle bottom)
     (collision collision))
  (if (eql (direction collision) :DOWN)
      (call-next-method)
      ;; else
      (progn
	(setf (colliding standing-rectangle) nil)
	(move-about moving-rectangle (desired-movement collision))
	(setf (colliding standing-rectangle) t))))

(defmethod on-collision
    ((moving-rectangle moving-enemy)
     (standing-rectangle player)
     (collision collision))
  (collide-blocks moving-rectangle
		  standing-rectangle
		  collision)
  (enemy-hits-player moving-rectangle
		     standing-rectangle
		     collision))
