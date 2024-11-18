
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize ()

  (initialize-item)

  (initialize-monster)
  
  (restore-game-file)

  (if (probe-file *game-user-file-name*)
      (open-game-user-file)
      (new-game-user-file))

  (setf (pos *grp*) +map-portal+)
  
  (setf *print-grp-states* t)
  (setf *process* @start-game@)

  )


