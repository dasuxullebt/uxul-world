#! /usr/bin/sbcl --script

(clc:clc-require :uxul-world)

(uxul-world:init-media)

(sb-ext:save-lisp-and-die "coredump"
	:executable
		t
	:toplevel
		#'(lambda (x)
			(unwind-protect
				(uxul-world::start-game #'uxul-world::make-testing-room)
				0)))

