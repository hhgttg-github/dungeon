
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @castle))
  (format t (c-str "+=-=-=-=-=-=-=-=-=-=-=+~%" :white :bg-black))
  (format t (c-str "|     C A S T L E     |~%" :white :bg-black))
  (format t (c-str "+=-=-=-=-=-=-=-=-=-=-=+~%" :white :bg-black))
  (princ #\newline)
  (format t "*grp* = ~a~%" *grp*)
  (format t "(item *grp*) = ~a~%" (bag *grp*))
  (when-let (c (prompt-char-choice
	      "G)uild S)tore M)aze Q)uit"
	      '(#\G #\S #\M #\Q)))
    (case c
      (#\G (setf *process* @guild@))
      (#\S (setf *process* @store@))
      (#\M (setf *process* @before-command@))
      (#\Q (setf *process* @quit-game@)))))
