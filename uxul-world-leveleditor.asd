;;; -*- lisp -*-

;;; Copyright 2009 Christoph Senjak

(defsystem "uxul-world-leveleditor"
  :description "Uxul World Leveleditor"
  :version "No Release Yet"
  :author "Christoph Senjak <firstName.secondName at googlemail.com>"
  :license "Copyright 2009 Christoph Senjak."
  :depends-on (#:uxul-world
	       #:ltk #:skippy #:flexi-streams)
  :components ((:file "uxul-world-leveleditor")
	       (:file "leveleditor"))
  :serial t)