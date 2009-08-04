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
    (setf (gethash 'uxul *leveleditor-images*) (stretched-base64-image uxul-world::|uxul_small1|))
    (setf (gethash 'leaf *leveleditor-images*) (stretched-base64-image uxul-world::|leaf|))
    (setf (gethash 'nasobem *leveleditor-images*) (stretched-base64-image uxul-world::|nasobem|))
    (setf (gethash 'blue-nasobem *leveleditor-images*) (stretched-base64-image uxul-world::|blue_nasobem|))
    (setf (gethash 'burning-marshmallow *leveleditor-images*) (stretched-base64-image uxul-world::|burning_marshmallow_ld1|))
    (setf (gethash 'gray-stone *leveleditor-images*) (stretched-base64-image uxul-world::|gray_stone|))
    (setf (gethash 'brown-stone *leveleditor-images*) (stretched-base64-image uxul-world::|brown_stone|))
    (setf (gethash 'empty *leveleditor-images*) (stretched-base64-image uxul-world::|empty|))
    (setf (gethash 'tulip *leveleditor-images*) (stretched-base64-image uxul-world::|tulip|))))

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

(defun create-room-from-item-list (item-list)
  (let*
      ((player (make-instance 'player
				:active t
				:visible t
				:redraw t))
       (room (make-instance 'room :width 0 :height 0
			    :graphic-centralizer player
			    :key-listener player
			    :key-up-function #'(lambda (key) (on-key-up player key))
			    :key-down-function #'(lambda (key) (on-key-down player key)))))
  (dolist (item item-list)
    (let ((y (car item))
	  (x (cadr item))
	  (type (caddr item)))
      (cond
	((eq type 'uxul)
	 (setf (x player) (* 128 x))
	 (setf (y player) (* 128 y))
	 (add-object player room))
	((eq type 'tulip)
	 (add-object (make-instance 'tulip
				    :x (* 128 x)
				    :y (* 128 y)) room))
	((eq type 'brown-stone)
	 (add-object (make-instance 'stone
				    :animation (make-animation 0 |brown_stone|)
				    :x (* 128 x)
				    :y (* 128 y)) room))
	((eq type 'gray-stone)
	 (add-object (make-instance 'stone
				    :animation (make-animation 0 |gray_stone|)
				    :x (* 128 x)
				    :y (* 128 y)) room))
	((eq type 'nasobem)
	 (add-object (make-instance 'simple-enemy
				    :x (* 128 x)
				    :y (* 128 y)) room))
	((eq type 'blue-nasobem)
	 (add-object (make-instance 'flying-nasobem
				    :x (* 128 x)
				    :y (* 128 y)) room))
	(T
	 (add-object (make-instance type
				 :x (* 128 x)
				 :y (* 128 y)) room)))))
  room))

(defun level-editor (&optional (level nil))
  (prepare-base64-images)
  (let ((item-table (make-hash-table :test 'equal)))
    ;;initialize given level
    (dolist (item level)
      (setf (gethash (cons (car item) (cadr item)) item-table) (caddr item)))
    
    (ltk:with-ltk ()
      (let*
	  ((uxul (load-image-into-tk (gethash 'uxul *leveleditor-images*)))
	   (leaf (load-image-into-tk (gethash 'leaf *leveleditor-images*)))
	   (nasobem (load-image-into-tk (gethash 'nasobem *leveleditor-images*)))
	   (blue-nasobem (load-image-into-tk (gethash 'blue-nasobem *leveleditor-images*)))
	   (burning-marshmallow (load-image-into-tk (gethash 'burning-marshmallow *leveleditor-images*)))
	   (gray-stone (load-image-into-tk (gethash 'gray-stone *leveleditor-images*)))
	   (brown-stone (load-image-into-tk (gethash 'brown-stone *leveleditor-images*)))
	   (empty (load-image-into-tk (gethash 'empty *leveleditor-images*)))
	   (tulip (load-image-into-tk (gethash 'tulip *leveleditor-images*)))
	   (current-upper-left (cons 0 0))
	   (current-chosen-object 'uxul)
	   (objects-and-arrows (make-instance 'ltk:frame))
	   (object-frame (make-instance 'ltk:frame :master objects-and-arrows))
	   (arrow-frame (make-instance 'ltk:frame :master objects-and-arrows))
	   (grid-frame (make-instance 'ltk:frame))
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
	   (btns (make-array '(16 16) :adjustable nil :element-type 'ltk:button)))
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
		       ((eq cval 'leaf)
			(config-button-image cbtn leaf))
		       ((eq cval 'nasobem)
			(config-button-image cbtn nasobem))
		       ((eq cval 'blue-nasobem)
			(config-button-image cbtn blue-nasobem))
		       ((eq cval 'burning-marshmallow)
			(config-button-image cbtn burning-marshmallow))
		       ((eq cval 'gray-stone)
			(config-button-image cbtn gray-stone))
		       ((eq cval 'brown-stone)
			(config-button-image cbtn brown-stone))
		       ((eq cval nil)
			(config-button-image cbtn empty))
		       ((eq cval 'tulip)
			(config-button-image cbtn tulip))
		       ((eq cval 'uxul)
			(config-button-image cbtn uxul)))))
		 (redraw-buttons ()
		   "Redraw all Buttons"
		     (dotimes (i 16)
		       (dotimes (j 16)
			 (redraw-button i j))))
		 (react (i j)
		   (let ((current-upper-x (car current-upper-left))
			 (current-upper-y (cdr current-upper-left)))
		     (cond
		       ((eq current-chosen-object 'burning-marshmallow)
			(setf (gethash (cons (+ i current-upper-x)
					     (+ j current-upper-y))
				       item-table) 'burning-marshmallow))
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
				 (setf current-chosen-object 'uxul)))
	  (ltk:grid nasobem-button 0 2)
	  (config-button-image nasobem-button nasobem)
	  (setf (ltk:command nasobem-button)
			     #'(lambda ()
				 (setf current-chosen-object 'nasobem)))
	  (ltk:grid blue-nasobem-button 0 3)
	  (config-button-image blue-nasobem-button blue-nasobem)
	  (setf (ltk:command blue-nasobem-button)
			     #'(lambda ()
				 (setf current-chosen-object 'blue-nasobem)))
	  (ltk:grid burning-marshmallow-button 0 4)
	  (config-button-image burning-marshmallow-button burning-marshmallow)
	  (setf (ltk:command burning-marshmallow-button)
			     #'(lambda ()
				 (setf current-chosen-object 'burning-marshmallow)))
	  (ltk:grid gray-stone-button 0 5)
	  (config-button-image gray-stone-button gray-stone)
	  (setf (ltk:command gray-stone-button)
			     #'(lambda ()
				 (setf current-chosen-object 'gray-stone)))
	  (ltk:grid brown-stone-button 0 6)
	  (config-button-image brown-stone-button brown-stone)
	  (setf (ltk:command brown-stone-button)
			     #'(lambda ()
				 (setf current-chosen-object 'brown-stone)))
	  (ltk:grid leaf-button 0 7)
	  (config-button-image leaf-button leaf)
	  (setf (ltk:command leaf-button)
			     #'(lambda ()
				 (setf current-chosen-object 'leaf)))

	  (ltk:grid tulip-button 0 8)
	  (config-button-image tulip-button tulip)
	  (setf (ltk:command tulip-button)
			     #'(lambda ()
				 (setf current-chosen-object 'tulip)))

	  (ltk:grid object-frame 0 0)
	  (ltk:pack objects-and-arrows)

	  (dotimes (i 16)
	    (dotimes (j 16)
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