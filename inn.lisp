
(defmethod game-process ((p @inn))
  (format t (c-str "Adventure's Inn" :black :bg-white))
  (princ #\newline)
  (print-grp-states)
  (let ((c (prompt-char-choice
	    "I)nspect L)eave"
	    '(#\I #\L))))
    (case c
      (#\I )
      (#\L))))
