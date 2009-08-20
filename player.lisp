;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

;; define the standard-class player, which will represent the
;; player.


(defclass player (game-object-with-animation)
  ((dont-ignore :initarg :dont-ignore
		:initform T
		:accessor dont-ignore
		:documentation "When we're testing whether we can go
		on or not without colliding, we will set this flag to
		nil, which means, that all collision-methods should
		ONLY set the player to the position when it collides,
		but NOT have any other effect (since its only a test)")
   (animation-left :accessor animation-left
		   :initform (make-animation 7
					     |uxul_small1|
					     |uxul_small2|))
   (animation-right :accessor animation-right
		    :initform (make-animation 7
					      |uxul_small3|
					      |uxul_small4|))
   (animation :initarg :animation
	      :accessor animation
	      :initform  (make-animation 7
					|uxul_small1|
					|uxul_small2|))
   (last-y :initarg :last-pos
	     :accessor last-y
	     :initform nil)
   (bounced :initarg :bounced
	    :accessor bounced
	    :initform nil)
   (jump-accel :accessor jump-accel
	       :initform -50)
   (mayjump :accessor mayjump
	    :initform t)
   (maycontjump :accessor maycontjump
		:initform t)
   (autojump :accessor autojump
	     :initform 0
	     :documentation "push jump-events even though no key is
	     pressed for n invocations.")
   (overjump :initarg :overjump
	     :accessor overjump
	     :initform 0
	     :documentation "How many Frames to overjump until movement. Default 0.")
   (overjumped :accessor overjumped
	       :initform 0
	       :documentation "DO NOT SET MANUALLY - counter for overjumped frames")
   (width :initarg :width
	  :accessor width
	  :initform 60)
   (height :initarg :height
	   :accessor height
	   :initform 75)
   (animation-translation :initarg :animation-translation
			  :accessor animation-translation
			  :initform (make-xy -40 -20))
   (key-pressed-up :initform nil :accessor key-pressed-up :initarg :key-pressed-up)
   (key-pressed-down :initform nil :accessor key-pressed-down :initarg :key-pressed-down)
   (key-pressed-left :initform nil :accessor key-pressed-left :initarg :key-pressed-left)
   (key-pressed-right :initform nil :accessor key-pressed-right :initarg :key-pressed-right)
   (go-down :initform 0 :accessor go-down :initarg :go-down)
   (go-right :initform 0 :accessor go-right :initarg :go-right)
   (power :initform 10 :accessor
   power :initarg :power :documentation "power - will be decreased if
   enemy touches.")
   (tulips :initform 0 :accessor tulips :initarg :tulips)
   (immortable :initform 0
	       :accessor immortable
	       :documentation "after hit by an enemy you wont be
	       wounded by another enemy for that ammount of
	       frames.")
   (keys :initform nil
	 :initarg :keys
	 :accessor keys
	 :documentation "List of Key-Dungeons of keys (i.e. for every
   key its key-dungeon is pushed on that list, for every door, its
   removed again).")  ))


;; Interaction with enemies
(defgeneric player-hits-enemy (player enemy &rest args)
  (:documentation
"To be called when a player collides with an enemy."))

(defmethod player-hits-enemy ((player t) (enemy t) &rest args)
  (declare (ignore args))
  "Shouldnt be called - warn only"
  (format t
	  "player-hits-enemy called with non-fitting classes: ~A ~A~%"
	  (class-name (class-of player))
	  (class-name (class-of enemy))))

(defgeneric enemy-hits-player (enemy player &rest args)
  (:documentation
"To be called when an enemy collides with a player."))

(defmethod enemy-hits-player ((enemy t) (player t) &rest args)
  (declare (ignore args))
  "Shouldnt be called - warn only"
  (format t
	  "player-hits-enemy called with non-fitting classes: ~A ~A~%"
	  (class-name (class-of enemy))
	  (class-name (class-of player))))

;; interaction with items

(defgeneric item-catch (item player &rest args)
  (:documentation "Obvious"))

(defmethod item-catch ((item t) (player t) &rest args)
  (declare (ignore args))
  "Do nothing, just warn."
  (format t "item-catch called with non-fitting classes: ~A ~A~%"
	  (class-name (class-of item))
	  (class-name (class-of player))))

(defmethod item-catch ((item key) (player player) &rest args)
  (declare (ignore args))
  (push (dungeon item) (keys player))
  (setf (visible item) nil)
  (setf (colliding item) nil))

(defmethod (setf animation) ((new-value animation) (object player))
  (setf (x new-value) (+ (x object) (x(animation-translation object))))
  (setf (y new-value) (+ (y object) (y(animation-translation object))))
  (call-next-method))

(defmethod on-key-down ((obj player) key)
  (cond
    ((sdl:key= key :SDL-KEY-UP)
     (setf (key-pressed-up obj) T))
    ((sdl:key= key :SDL-KEY-DOWN)
     (setf (key-pressed-down obj) T))
    ((sdl:key= key :SDL-KEY-LEFT)
     (setf (key-pressed-left obj) T)
     (setf (animation obj) (animation-left obj))
     (setf (animation-translation obj) (make-xy -40 -20))
     (ensure-playing (animation obj))
     )
    ((sdl:key= key :SDL-KEY-RIGHT)
     (setf (key-pressed-right obj) T)
     (setf (animation obj) (animation-right obj))
     (ensure-playing (animation obj))
     (setf (animation-translation obj) (make-xy -20 -20))
     )
    ))

(defmethod on-key-up ((obj player) key)
  (cond
    ((sdl:key= key :SDL-KEY-UP )
     (setf (key-pressed-up obj) NIL))
    ((sdl:key= key :SDL-KEY-DOWN)
     (setf (key-pressed-down obj) NIL))
    ((sdl:key= key :SDL-KEY-LEFT)
     (setf (key-pressed-left obj) NIL)
     (ensure-pause (animation obj)))
    ((sdl:key= key :SDL-KEY-RIGHT)
     (setf (key-pressed-right obj) NIL)
     (ensure-pause (animation obj)))))

(defmethod invoke ((obj player))
  "Do whatever a player does ^^"

  (cond
    ((bounced obj)
     (setf (bounced obj) nil))
    ((and
      (last-y obj)
      (< (last-y obj) (y obj)))
     (setf (mayjump obj) nil)))

  (setf (last-y obj) (y obj))

  ;; SIMPLE GRAVITY HACK
  (setf (key-pressed-down obj) (not (key-pressed-up obj)))

  (if (not (zerop (immortable obj))) (decf (immortable obj)))

  (let ((go-left (if (key-pressed-left obj) 10 0))
	(go-right (if (key-pressed-right obj) 10 0))
	(go-up 30))
    (labels ((jump ()
	      (cond ((mayjump obj)
	      (setf (mayjump obj) nil)
	      (setf (maycontjump obj) t)
	      (setf go-up (jump-accel obj))
	      (setf (jump-accel obj) -49))
	     ((maycontjump obj)
	      (setf go-up (jump-accel obj))
	      (incf (jump-accel obj) 3)
	      (when (zerop (jump-accel obj))
		  (setf (maycontjump obj) nil)
		  (setf (jump-accel obj) -50))))))
      (cond
	((key-pressed-up obj)
	 (jump))
	((> (autojump obj) 0)
	 (jump)
	 (decf (autojump obj)))
	(T (setf (maycontjump obj) nil)))

    (move-about obj (make-xy (- go-right go-left) go-up)))))