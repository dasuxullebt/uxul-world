;;; -*- lisp -*-

;;; Copyright 2009 Christoph Senjak

(defsystem "uxul-world"
  :description "Uxul World - A simple Jump'N'Run"
  :version "No Release Yet"
  :author "Christoph Senjak <firstName.secondName at googlemail.com>"
  :license "Copyright 2009 Christoph Senjak."
  :depends-on (#:lispbuilder-sdl #:cl-opengl
				 #:closer-mop
				 #:cl-fad
                                 #:lispbuilder-sdl)
  :components ((:file "uxul-world")
               (:file "constants")
               (:file "macros")
	       (:file "bmp")
	       (:file "opengl")
               (:file "xy-coordinates")
               (:file "collision")
               (:file "files")
               (:file "animation")
               (:file "functions")
               (:file "game-object")
               (:file "game-object-with-animation")
	       (:file "elementary-classes")
               (:file "small-classes")
               (:file "player")
	       (:file "simple-enemy")
	       (:file "flying-nasobem")
	       (:file "burning-marshmallow")
               (:file "on-collision")
               (:file "room")
	       (:file "objectarray")
               (:file "add-object")
	       (:file "draw")
               (:file "game")
	       (:file "testing-room")
	       )
  :serial t)
