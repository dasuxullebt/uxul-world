;; Copyright 2010 Christoph Senjak

(in-package :uxul-world)

;; "Binding" for the "convert"-Program

(defparameter *convert* #P"C:\\Program Files (x86)\\ImageMagick-6.6.2-Q16\\convert.exe")

(defun run-convert (arguments in)
  "Return output of convert"
  (let* ((p (sb-ext:run-program *convert* arguments
				      :wait nil
				      :input :stream
				      :output :stream))
	 (pin (sb-ext:process-input p))
	 (pou (sb-ext:process-output p))
	 (ret '()))
    (loop for byte across in do
	 (progn
	   (format t "doing~%")
	   (write-byte byte pin)
	   (loop while (listen pou) do
		;; this read should never fail and never be eof
		(format t "reading 1~%")
		(push (read-byte pou) ret))))
    (format t "finishing out, closing~%")
    (finish-output pin)
    (close pin)
    (let ((c 0))
      (loop while (setf c (read-byte pou nil nil)) do
	   (format t "reading 2~%")
	   (push c ret)))
    ret))

(defun resize-image (bytes x y)
  (run-convert (list "-scale" (format nil "~dx~d" x y) "-" "-") bytes))