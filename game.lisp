;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world)

(defparameter *cfont* nil)

(defun run-testing-room ()
  (start-game :room-function #'make-testing-room))

(defun run-room (item-list)
  (start-game :room-function
	      #'(lambda () (create-room-from-item-list item-list))))

(defun start-game (&key (music nil)
		   (room-function #'make-additional-testing-room)
		   (15-fps nil))
  "Start the Game: Call room-function for getting the room-object to
run. Music is ignored so far. 15-fps makes only every second frame be
drawn (for very slow computers)"
  (sdl:set-video-driver "directx")
     (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
       (sdl:window +screen-width+ +screen-height+
		   :title-caption "Uxul World"
		   :icon-caption "Uxul World"
		   ;:opengl T
		   :flags (logior sdl:sdl-hw-accel  sdl:sdl-hw-surface)
		   :flags (logior sdl:sdl-hw-surface) #| sdl:sdl-fullscreen )|# 
)
       ;;(if music (sdl-mixer:OPEN-AUDIO :frequency 44100))
       (let ((*graphics-table* (make-hash-table :test #'equal)))
	 (if 15-fps
	     (setf (sdl:frame-rate) 15)
	     (setf (sdl:frame-rate) 30))
	 
	 (setf *current-room* (funcall room-function))

	 (sdl:clear-display (sdl:color :r 0 :g 0 :b 0));; :update-p nil)

	 ;;(if music (sdl-mixer:play-sample levelmusic))
      
	 (sdl:with-events ()
	   (:quit-event () 
			#|(if music
			(progn (sdl-mixer:halt-music)
			(sdl-mixer:halt-sample :channel t)
			(sdl-mixer:free levelmusic)
			(sdl-mixer:close-audio))
			t
			)|# t)
	   (:key-down-event (:key key)
			    (cond
			      ((sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event))
			      ((sdl:key= key :SDL-KEY-O)
			       (setf *zoom-ash*
				     (max -3 (1- *zoom-ash*))))
			      ((sdl:key= key :SDL-KEY-I)
			       (setf *zoom-ash*
				     (min 0 (1+ *zoom-ash*))))
			      (T
			       (on-key-down *current-room* key))))
	   (:key-up-event (:key key)
			  (on-key-up *current-room* key))
	   (:idle
	    (progn
	      (invoke *current-room*)
	      (when 15-fps
		(invoke *current-room*))
	      (sdl:clear-display (sdl:color :r 128 :g 128 :b 128)); :update-p nil)
	      (draw *current-room*)
	      (sdl:update-display)
	 ))))))


;; For Debugging

(defun preview-animation (frameskip &rest images)

     (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
       (sdl:window +screen-width+ +screen-height+
		   :title-caption "Uxul World"
		   :icon-caption "Uxul World"
		   :flags (logior sdl:sdl-hw-accel)
		   #| :flags (logior sdl:sdl-hw-surface sdl:sdl-fullscreen )|#  )
       (let ((*graphics-table*
	      #-ecl (trivial-garbage:make-weak-hash-table
		     :weakness :value
		     :test #'equal)
	      #+ecl (make-hash-table :test #'equal)
	      )
	     (my-anim (apply #'make-animation frameskip images))
	     )
	 
	 (setf (sdl:frame-rate) 30)
	 (sdl:clear-display (sdl:color :r 64 :g 64 :b 46));; :update-p nil)
      
	 (sdl:with-events ()
	   (:quit-event () t)
	   (:key-down-event (:key key)
			    (cond
			      ((sdl:key= key :SDL-KEY-ESCAPE)
			       (sdl:push-quit-event))))
	   (:idle
	    (progn
	      (sdl:clear-display (sdl:color :r 64 :g 64 :b 46));; :update-p nil)


	      (draw my-anim)
	      
	      (sdl:update-display)
	 ))))))
