;;; -*- lisp -*-

;;; Copyright 2009 Christoph Senjak

(defsystem "uxul-world"
  :description "Uxul World - A simple Jump'N'Run"
  :version "No Release Yet"
  :author "Christoph Senjak <firstName.secondName at googlemail.com>"
  :license "Copyright 2009 Christoph Senjak."
  :depends-on (#:lispbuilder-sdl #:closer-mop
				 #:cl-fad
				 ;#:asdf
				 #:ltk
				 #:lisp-magick
                                 #:lispbuilder-sdl-image
                                 #:trivial-garbage)
  :components ((:file "package")
               (:file "macros")
               (:file "constants")
               (:file "xy-coordinates")
               (:file "collision")
               (:file "files")
	       (:file "leveleditor")
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
