
;;;;====================================
;;;;
;;;; CAMP

(defmethod game-process ((p @camp))
  (format t (c-str "=-=-=-=-=-=-=-~%" :white :bg-black))
  (format t (c-str "   C A M P    ~%" :white :bg-black))
  (format t (c-str "=-=-=-=-=-=-=-~%" :white :bg-black))
  (princ #\newline)
  (print-grp-states)
  (princ #\newline)
  (when-let (c (prompt-char-choice
	      "I)nspect S)pell B)ag L)eave"
	      '(#\I #\S #\B #\E #\D #\L)))
    (case c
      (#\I (setf *process* @camp-inspect@))
      (#\S (setf *process* @camp-spell@))
      (#\B (setf *process* @camp-bag@))
      (#\L (setf *process* @after-command@)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-inspect))
  (print-grp-states)
  (setf *process* @camp@)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-spell))
  (format t "Cast Spell~%")
  (setf *process* @camp@)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-bag))
  (if (bag *grp*)
      (progn
	(print-bag-with-number *grp*)
	(when-let (c (prompt-char-choice
		      "E)quip U)se D)rop L)eave"
		      '(#\E #\U #\D #\L)))
		  (case c
		    (#\E (setf *process* @camp-bag-equip@))
		    (#\U (setf *process* @camp-bag-use@))
		    (#\D (setf *process* @camp-bag-drop@))
		    (#\L (setf *process* @camp@)))))
      (progn
	(format t "~%      パーティの持ち物は、ありません。~%~%")
	(setf *process* @camp@))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-bag-equip))
  (when-let (f (prompt-select-member "誰が装備しますか?" *grp*))
    (let ((q-equip nil))
      (loop until q-equip
	 do
	   (print-bag-with-number *grp*)
	   (if-let (n (prompt-select-in-bag "どれを装備しますか?" *grp*))
	       (equip-from-bag f (1- n) *grp*)
	     (setf q-equip t)))))
  (setf *process* @camp-bag@))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-bag-use))
  (format t "Use Item~%")
  (setf *process* @camp-bag@))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @camp-bag-drop))
  (format t "Drop Item~%")
  (when-let ((n
	      (prompt-number-between "どれを捨てますか？"
				   1 (length (bag *grp*)))))
	    (format t "~aを捨てました。~%" (name (nth (1- n) (bag *grp*))))
	    (setf (bag *grp*) (remove-nth (1- n) (bag *grp*))))
  
  (setf *process* @camp-bag@))

