
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @start-game))
  (let ((s "N)ew Game Q)uit~%")             ; New/Quit の二択
	(choice '(#\N #\Q)))
    
    (format t "start-game~%")

    (when (probe-file *game-user-file-name*)          ; game.data存在すれば、New/Start/Quit
      (setf s "N)ew Game   S)tart Game   Q)uit~%")
      (setf choice  '(#\N #\S #\Q)))

    (format t s)
    (if-let (c (prompt-char-choice ">" choice))
      (case c
	(#\N (new-game-user-file))
	(#\S (setf *process* @castle@))
	(#\Q (setf *process* @quit-game@))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @quit-game))
  (close-game-user-file *guild* *grp*)
  (setf *quit-game* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main-loop ()
  ;;  (format t "~a~%" *process*)
  ;;  (format t "pos = ~a~%" (pos *grp*))
  (princ #\newline)
  (when *print-grp-states*
    (print-grp-states))
  (princ #\newline)
  (game-process *process*))

(defun main ()
  (initialize)
  
  (loop until *quit-game*
   	do (main-loop))
  (setf *quit-game* nil))
