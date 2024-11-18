
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @before-battle))
  (format t "~a との戦闘~%" (foe @before-battle@))
  (setf (fellow *monster-grp*) (list (create-monster (foe @before-battle@))))
  (format t "~a ~a ~a/~a =%"
	  (name (car (fellow *monster-grp*)))
	  (dice (car (fellow *monster-grp*)))
	  (hp   (car (fellow *monster-grp*)))
	  (hpmax (car (fellow *monster-grp*))))
  (setf *process* @battle-command@))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @battle-command))
  (when-let (c (prompt-char-choice
	      "A)ttack R)un L)eave"
	      '(#\A #\R #\L)))
    (case c
      (#\A (setf *process* @start-battle@))
      (#\R (setf *process* @run-from-battle@))
      (#\L (setf *process* @main-command@)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @start-battle))
  (let*((player-strike  (strike-by-player (fellow *grp*)))
	(monster-strike (strike-by-monster (car (fellow *monster-grp*))))
	(damage         (- player-strike monster-strike)))
    (format t "player-hp = ~a~%" (hp (fellow *grp*)))
    (format t "monster-hp = ~a~%" (hp (car (fellow *monster-grp*))))
    (format t "player-strike = ~a~%" player-strike)
    (format t "monster-strike = ~a~%" monster-strike)
    (format t "damage = ~a~%" damage)
    (cond ((plusp damage) ; player > monster
	   (format t "Playerによる攻撃   ~aに~aポイントのダメージ!"
		   (name (car (fellow *monster-grp*))) damage)
	   (damage-to-monster (car (fellow *monster-grp*)) damage))
	  ((minusp damage) ; player < monster
	   (setf damage (abs damage))
	   (format t "~aによる攻撃   Playerに~aポイントのダメージ!"
		   (name (car (fellow *monster-grp*))) damage)
	   (damage-to-player (fellow *grp*) damage))
	  (t
	   (format t "拮抗 双方ダメージなし!")))
    (if (dead-monsterp (car (fellow *monster-grp*)))
	(progn
	  (setf (foe @defeat-monster@) (fellow *monster-grp*))
	  (setf *process* @defeat-monster@))
	(if (dead-playerp (fellow *grp*))
	    (setf *process* @game-over@)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @defeat-monster))
  (format t "~a を倒した!~%" (foe @defeat-monster@))
  (setf *process* @main-command@))

(defmethod game-process ((p @game-over))
  (format t "プレーヤーは死亡しました。~%")
  (setf *process* @quit-game@))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @run-from-battle))
  (format t "逃げだした...~%")
  (setf *process* @main-command@))
