
;---------------------------------------

(defun draw-around (p)
  "p : grp"
  (draw-3x3 (pos p))
  (draw-message (pos p)))

;=======================================
;;;;
;;;; BEFORE-COMMAND

(defmethod game-process ((p @before-command))
  (draw-around *grp*)
  (if-let (enc (encountp *map* *grp* *encount-table*))
    (progn
      (setf (foe @before-battle@) enc)
      (setf *process* @before-battle@))
    (setf *process* @main-command@)))

;=======================================
;;;;
;;;; MAIN-COMMAND

(defun move-player (dx dy)
  (let ((new-pos (over-edge (pos *grp*) dx dy)))
    (if (passable (aref *map* new-pos))
	(setf (pos *grp*) new-pos)
	(format t "いてっ! 壁だ...~%")))
  (setf *process* @after-command@))

;---------------------------------------

(defmethod game-process ((p @main-command))
  (when-let ((c (prompt-char-choice ">" '(#\C #\N #\E #\W #\S #\Q))))
    (case c
      (#\C (setf *process* @camp@))
      (#\N (move-player 0 -1))
      (#\E (move-player 1  0))
      (#\W (move-player -1 0))
      (#\S (move-player 0  1))
      (#\Q (setf *process* @quit-game@)))))

;=======================================
;;;;
;;;; AFTER-COMMAND

(defmethod game-process ((p @after-command))
  (setf *process* @before-command@))
