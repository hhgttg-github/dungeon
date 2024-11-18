
(defclass @quit-game-yn (process)())
(defparameter @quit-game-yn@ (make-instance '@quit-game-yn))

(defun quit-game ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process((p @quit-game-yn))
  (when (y-or-n-p "ゲームを終了しますか?")
    (setf *process* @quit-game@)
    (quit-game)))


  
