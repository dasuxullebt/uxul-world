;;; Copyright 2009-2011 Christoph Senjak

(defpackage #:uxul-world
	(:use
	 #:cl)
	(:shadow #:room)
	(:export
	 #:init-media
	 #:level-editor
	 #:create-room-from-item-list
	 #:start-game
	 #:run-room
	 #:run-testing-room))
