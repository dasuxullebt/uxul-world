;;; Copyright 2009 Christoph Senjak

(in-package :uxul-world-leveleditor)

(defparameter *leveleditor-images* nil)

(defun stretched-base64-image (img)
  "Call ImageMagick to resize that file to 32x32."
  (lisp-magick:with-magick-wand (mywand)
    (lisp-magick::magick-read-image-blob mywand img)
    (lisp-magick::magick-resize-image mywand 32 32 #x00000000 1d0)
    (lisp-magick::magick-set-format mywand "gif")
    (base64-encode-byteseq (lisp-magick::magick-get-image-blob mywand))))

(defun prepare-base64-images (&optional (care-about-initialization *leveleditor-images*))
  (when (not care-about-initialization)
    (setf *leveleditor-images* (make-hash-table))
    (setf (gethash 'uxul-world::uxul *leveleditor-images*) (stretched-base64-image uxul-world::|uxul_small1|))
    (setf (gethash 'uxul-world::leaf *leveleditor-images*) (stretched-base64-image uxul-world::|leaf|))
    (setf (gethash 'uxul-world::nasobem *leveleditor-images*) (stretched-base64-image uxul-world::|nasobem|))
    (setf (gethash 'uxul-world::blue-nasobem *leveleditor-images*) (stretched-base64-image uxul-world::|blue_nasobem|))
    (setf (gethash 'uxul-world::burning-marshmallow *leveleditor-images*) (stretched-base64-image uxul-world::|burning_marshmallow_ld1|))
    (setf (gethash 'uxul-world::gray-stone *leveleditor-images*) (stretched-base64-image uxul-world::|gray_stone|))
    (setf (gethash 'uxul-world::brown-stone *leveleditor-images*) (stretched-base64-image uxul-world::|brown_stone|))
    (setf (gethash 'uxul-world::empty *leveleditor-images*) (stretched-base64-image uxul-world::|empty|))
    (setf (gethash 'uxul-world::tulip *leveleditor-images*) (stretched-base64-image uxul-world::|tulip|))
    (setf (gethash 'uxul-world::door *leveleditor-images*) (stretched-base64-image uxul-world::|door|))
    (setf (gethash 'uxul-world::key *leveleditor-images*) (stretched-base64-image uxul-world::|key|))
))

(defun load-image-into-tk (png-base64)
  "return a tkobject with this image"
  (let ((name (ltk::create-name)))
    (ltk:format-wish "set ~A [ image create photo -data \"~A\" ]"
		     name png-base64)
    (make-instance 'ltk:tkobject :name name)))

(defun config-button-image (button tkobject)
  (ltk:format-wish "~A configure -image $~A" (ltk::widget-path button) (ltk::name tkobject)))

(defun item-table-to-list (item-table)
  "Special Function vor level-editor. Returns a list of lists of the
form (x y object)."
  (let ((ret nil))
    (maphash #'(lambda (key val)
		 (when val
		   (push (list (car key) (cdr key) val) ret)))
	     item-table)
    ret))

(defun level-editor (&optional (level nil) (width 16) (height 16))
  (prepare-base64-images)
  (let ((item-table (make-hash-table :test 'equal)))
    ;;initialize given level
    (dolist (item level)
      (setf (gethash (cons (car item) (cadr item)) item-table) (caddr item)))
    
    (ltk:with-ltk ()
      (let*
	  ((uxul (load-image-into-tk (gethash 'uxul-world::uxul *leveleditor-images*)))
	   (leaf (load-image-into-tk (gethash 'uxul-world::leaf *leveleditor-images*)))
	   (nasobem (load-image-into-tk (gethash 'uxul-world::nasobem *leveleditor-images*)))
	   (blue-nasobem (load-image-into-tk (gethash 'uxul-world::blue-nasobem *leveleditor-images*)))
	   (burning-marshmallow (load-image-into-tk (gethash 'uxul-world::burning-marshmallow *leveleditor-images*)))
	   (gray-stone (load-image-into-tk (gethash 'uxul-world::gray-stone *leveleditor-images*)))
	   (brown-stone (load-image-into-tk (gethash 'uxul-world::brown-stone *leveleditor-images*)))
	   (empty (load-image-into-tk (gethash 'uxul-world::empty *leveleditor-images*)))
	   (tulip (load-image-into-tk (gethash 'uxul-world::tulip *leveleditor-images*)))
	   (key (load-image-into-tk (gethash 'uxul-world::key *leveleditor-images*)))
	   (door (load-image-into-tk (gethash 'uxul-world::door *leveleditor-images*)))
	   (current-upper-left (cons 0 0))
	   (current-chosen-object 'uxul)
	   (objects-and-arrows (make-instance 'ltk:frame))
	   (grid-frame (make-instance 'ltk:toplevel))
	   (object-frame (make-instance 'ltk:frame :master objects-and-arrows))
	   (arrow-frame (make-instance 'ltk:frame :master objects-and-arrows))
	   (right-button (make-instance 'ltk:button :text ">"
					:master arrow-frame))
	   (left-button (make-instance 'ltk:button :text "<"
				       :master arrow-frame))
	   (up-button (make-instance 'ltk:button :text "/\\"
				     :master arrow-frame))
	   (down-button (make-instance 'ltk:button :text "\\/"
				       :master arrow-frame))
	   (rright-button (make-instance 'ltk:button :text ">>"
					:master arrow-frame))
	   (lleft-button (make-instance 'ltk:button :text "<<"
				       :master arrow-frame))
	   (uup-button (make-instance 'ltk:button :text "//\\\\"
				     :master arrow-frame))
	   (ddown-button (make-instance 'ltk:button :text "\\\\//"
				       :master arrow-frame))
	   (uxul-button (make-instance 'ltk:button :text ""
				       :master object-frame))
	   (nasobem-button (make-instance 'ltk:button :text ""
					  :master object-frame))
	   (blue-nasobem-button (make-instance 'ltk:button :text ""
					       :master object-frame))
	   (burning-marshmallow-button (make-instance 'ltk:button :text ""
						      :master object-frame))
	   (gray-stone-button (make-instance 'ltk:button :text ""
					     :master object-frame))
	   (brown-stone-button (make-instance 'ltk:button :text ""
				       :master object-frame))
	   (empty-button (make-instance 'ltk:button :text ""
					:master object-frame))
	   (tulip-button (make-instance 'ltk:button :text ""
					:master object-frame))
	   (leaf-button (make-instance 'ltk:button :text ""
				       :master object-frame))
	   (key-button (make-instance 'ltk:button :text ""
				       :master object-frame))
	   (door-button (make-instance 'ltk:button :text ""
				       :master object-frame))
	   (btns (make-array (list width height) :adjustable nil :element-type 'ltk:button)))
	(labels ((redraw-button (i j)
		   "Redraw Button (i, j)"
		   (let* ((current-upper-x (car current-upper-left))
			  (current-upper-y (cdr current-upper-left))
			  (cval (gethash (cons (+ i current-upper-x)
					       (+ j current-upper-y))
					item-table nil))
			 (cbtn (aref btns i j)))
		     (if (listp cval)
			 (setf cval (car cval)))
		     (cond
		       ((eq cval 'uxul-world::leaf)
			(config-button-image cbtn leaf))
		       ((eq cval 'uxul-world::nasobem)
			(config-button-image cbtn nasobem))
		       ((eq cval 'uxul-world::blue-nasobem)
			(config-button-image cbtn blue-nasobem))
		       ((eq cval 'uxul-world::burning-marshmallow)
			(config-button-image cbtn burning-marshmallow))
		       ((eq cval 'uxul-world::gray-stone)
			(config-button-image cbtn gray-stone))
		       ((eq cval 'uxul-world::brown-stone)
			(config-button-image cbtn brown-stone))
		       ((eq cval nil)
			(config-button-image cbtn empty))
		       ((eq cval 'uxul-world::tulip)
			(config-button-image cbtn tulip))
		       ((eq cval 'uxul-world::door)
			(config-button-image cbtn door))
		       ((eq cval 'uxul-world::key)
			(config-button-image cbtn key))
		       ((eq cval 'uxul-world::uxul)
			(config-button-image cbtn uxul)))))
		 (redraw-buttons ()
		   "Redraw all Buttons"
		     (dotimes (i width)
		       (dotimes (j height)
			 (redraw-button i j))))
		 (react (i j)
		   (let ((current-upper-x (car current-upper-left))
			 (current-upper-y (cdr current-upper-left)))
		     (cond
		       ((eq current-chosen-object 'uxul-world::burning-marshmallow)
			(setf (gethash (cons (+ i current-upper-x)
					     (+ j current-upper-y))
				       item-table) 'uxul-world::burning-marshmallow))
		       (t
			(setf (gethash (cons (+ i current-upper-x)
					     (+ j current-upper-y))
				       item-table) current-chosen-object)))
		     (redraw-button i j)))
		 (move-field-about (i j)
		   (let ((current-upper-y (car current-upper-left))
			 (current-upper-x (cdr current-upper-left)))
		     (setf current-upper-left (cons (+ i current-upper-y) (+ j current-upper-x))))
		   (redraw-buttons)))
	  (ltk:pack grid-frame)
	  (ltk:grid arrow-frame 0 1)
	  (ltk:grid left-button 1 0)
	  (setf (ltk:command left-button) #'(lambda () (move-field-about 0 1)))
	  (ltk:grid lleft-button 2 0)
	  (setf (ltk:command lleft-button) #'(lambda () (move-field-about 0 15)))
	  (ltk:grid right-button 1 2)
	  (setf (ltk:command right-button) #'(lambda () (move-field-about 0 -1)))
	  (ltk:grid rright-button 0 2)
	  (setf (ltk:command rright-button) #'(lambda () (move-field-about 0 -15)))
	  (ltk:grid up-button 0 1)
	  (setf (ltk:command up-button) #'(lambda () (move-field-about 1 0)))
	  (ltk:grid uup-button 0 0)
	  (setf (ltk:command uup-button) #'(lambda () (move-field-about 15 0)))
	  (ltk:grid down-button 2 1)
	  (setf (ltk:command down-button) #'(lambda () (move-field-about -1 0)))
	  (ltk:grid ddown-button 2 2)
	  (setf (ltk:command ddown-button) #'(lambda () (move-field-about -15 0)))

	  (ltk:grid empty-button 0 0)
	  (config-button-image empty-button empty)
	  (setf (ltk:command empty-button)
			     #'(lambda ()
				 (setf current-chosen-object nil)))
	  (ltk:grid uxul-button 0 1)
	  (config-button-image uxul-button uxul)
	  (setf (ltk:command uxul-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::uxul)))
	  (ltk:grid nasobem-button 0 2)
	  (config-button-image nasobem-button nasobem)
	  (setf (ltk:command nasobem-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::nasobem)))
	  (ltk:grid blue-nasobem-button 0 3)
	  (config-button-image blue-nasobem-button blue-nasobem)
	  (setf (ltk:command blue-nasobem-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::blue-nasobem)))
	  (ltk:grid burning-marshmallow-button 0 4)
	  (config-button-image burning-marshmallow-button burning-marshmallow)
	  (setf (ltk:command burning-marshmallow-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::burning-marshmallow)))
	  (ltk:grid gray-stone-button 0 5)
	  (config-button-image gray-stone-button gray-stone)
	  (setf (ltk:command gray-stone-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::gray-stone)))
	  (ltk:grid brown-stone-button 0 6)
	  (config-button-image brown-stone-button brown-stone)
	  (setf (ltk:command brown-stone-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::brown-stone)))
	  (ltk:grid leaf-button 0 7)
	  (config-button-image leaf-button leaf)
	  (setf (ltk:command leaf-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::leaf)))

	  (ltk:grid tulip-button 0 8)
	  (config-button-image tulip-button tulip)
	  (setf (ltk:command tulip-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::tulip)))

	  (ltk:grid key-button 0 9)
	  (config-button-image key-button key)
	  (setf (ltk:command key-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::key)))

	  (ltk:grid door-button 0 10)
	  (config-button-image door-button door)
	  (setf (ltk:command door-button)
			     #'(lambda ()
				 (setf current-chosen-object 'uxul-world::door)))

	  (ltk:grid object-frame 0 0)
	  (ltk:pack objects-and-arrows)

	  (dotimes (i width)
	    (dotimes (j height)
	      (let ((cbtn
		     (make-instance 'ltk:button
				    :master grid-frame
				    :text "")))
		(setf (ltk:command cbtn) (let ((i i) (j j)) #'(lambda () (react i j))))
		(config-button-image cbtn empty)
		(setf (aref btns i j) cbtn)
		(ltk:grid cbtn i j))))
	  (redraw-buttons))))
    (item-table-to-list item-table)))


(defun get-base64-char-for-number (i)
  (declare (type (integer 0 63) i))
  (elt "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" i))

(defun base64-encode-threebytes (byte1 byte2 byte3)
  (declare (type (unsigned-byte 8) byte1 byte2 byte3))
  (coerce
   (list
    (get-base64-char-for-number (logand #b111111 (ash byte1 -2)))
    (get-base64-char-for-number (logand #b111111 (+ (ash (ash byte1 6) -2) (ash byte2 -4))))
    (get-base64-char-for-number (logand #b111111 (+ (ash (ash byte2 4) -2) (ash byte3 -6))))
    (get-base64-char-for-number (logand #b111111 (ash (ash byte3 2) -2)))) 'string))  


(defun base64-encode-bytelist (bytelist &optional (ret ""))
  (if bytelist
      (if (cdr bytelist)
	  (if (cddr bytelist)
	      (base64-encode-bytelist
	       (cdddr bytelist)
	       (concatenate 'string
			    ret
			    (base64-encode-threebytes
			     (car bytelist)
			     (cadr bytelist)
			     (caddr bytelist))))
	      ;;else (genau zwei elemente)
	      (concatenate 'string ret			   
			   (base64-encode-threebytes
			    (car bytelist)
			    (cadr bytelist)
			    0)
			   "="))
	  ;;else (genau ein element)
	  (concatenate 'string ret			   
		       (base64-encode-threebytes
			(car bytelist) 0 0)
		       "=="))
      ;;else (kein element)
      ret))


(defun base64-encode-byteseq (byteseq &optional (ret ""))
  (case (length byteseq)
    (0 ret)
    (1 (concatenate 'string ret			   
		    (base64-encode-threebytes
		     (elt byteseq 0) 0 0) "=="))
    (2 (concatenate 'string ret			   
		    (base64-encode-threebytes
		     (elt byteseq 0)
		     (elt byteseq 1)
		     0)
		    "="))
    (t (base64-encode-byteseq
	(subseq byteseq 3)
	(concatenate 'string
		     ret
		     (base64-encode-threebytes
		      (elt byteseq 0)
		      (elt byteseq 1)
		      (elt byteseq 2)))))))