(defclass player-class ()
  ((name    :initarg :name    :accessor name    :initform "NONAME")
   (job     :initarg :job     :accessor job     :initform :no-job)
   (lvl     :initarg :lvl     :accessor lvl     :initform 1)
   (ex      :initarg :ex      :accessor ex      :initform 0)
   (hp      :initarg :hp      :accessor hp      :initform 10)
   (hpmax   :initarg :hpmax   :accessor hpmax   :initform 10)
   (mp      :initarg :mp      :accessor mp      :initform 0)
   (mpmax   :initarg :mpmax   :accessor mpmax   :initform 0)
   (str     :initarg :str     :accessor str     :initform nil)
   (dex     :initarg :dex     :accessor dex     :initform nil)
   (mag     :initarg :mag     :accessor mag     :initform nil)
   (strike  :initarg :strike  :accessor strike  :initform nil)
   (defence :initarg :defence :accessor defence :initform nil)
   (item    :initarg :item    :accessor item    :initform nil)
   (e-slots :initarg :e-slots :accessor e-slots :initform
	    '((helm   . nil)
	      (melee1 . nil)
	      (melee2 . nil)
	      (shield . nil)
	      (armor  . nil)
	      (boots  . nil)
	      (gloves . nil)))))

(defclass grp-class ()
  ((pos     :initarg :pos     :accessor pos     :initform nil)
   ;;城ならnil, ダンジョン内なら場所番号
   (fellow  :initarg :fellow  :accessor fellow  :initform nil)
   (gold    :initarg :gold    :accessor gold    :initform 100)
   (bag     :initarg :bag     :accessor bag     :initform nil)))

;;;;====================================
;;;;
;;;; JOB BIT

(defparameter +nojob+  #b0000)
(defparameter +fighter+ #b0001)
(defparameter +cleric+  #b0010)
(defparameter +thief+   #b0100)
(defparameter +mage+    #b0001)

;;;;====================================
;;;;
;;;; CREATE PLAYER CHARACTER

(defun create-player-character (j)
  (let ((p (make-instance 'plaer-class :job j)))
    (case j
      (:fighter ((setf (str p) (make-dice 1 8 0))
		 (setf (dex p) (make-dice 1 4 0))
		 (setf (mag p) (make-dice 1 2 0))))
      (:cleric  ((setf (str p) (make-dice 1 6 0))
		 (setf (dex p) (make-dice 1 6 0))
		 (setf (mag p) (make-dice 1 6 0))))
      (:thief   ((setf (str p) (make-dice 1 4 0))
		 (setf (dex p) (make-dice 1 8 0))
		 (setf (mag p) (make-dice 1 4 0))))
      (:mage    ((setf (str p) (make-dice 1 2 0))
		 (setf (dex p) (make-dice 1 6 0))
		 (setf (mag p) (make-dice 1 8 0))))
      (t        ((setf (str p) (make-dice 1 4 0))
		 (setf (dex p) (make-dice 1 4 0))
		 (setf (mag p) (make-dice 0 0 0)))))

    (let ((h (roll (str p)))
	  (m (roll (mag p))))
      (setf (hp    p) h)
      (setf (hpmax p) h)
      (setf (mp    p) m)
      (setf (mpmax p) m))
    p))

(defun initilize-equipment (p)
  (case (job p)
    (:fighter ())
    (:cleric  ())
    (:thief   ())
    (:mage    ())
    (t        ())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *grp* (make-instance 'grp-class))

;;;;====================================
;;;;
;;;; PRINT PLAYER DATA

(defun print-fellow (f)
  (format t "~15@a : ~a~%"            "Name"      (name f))
  (format t "~15@a : ~a ~15@a : ~a~%" "Level"     (lvl f) "Exp." (ex f))
  (format t "~15@a : ~a / ~a~%"       "Hp"        (hp f) (hpmax f))
  (format t "~15@a : ~a~%"            "Strength"  (str f))
  (format t "~15@a : ~a~%"            "Dexterity" (dex f))
  (format t "~15@a : ~a~%"            "Magic"     (mag f))
  (format t "~15@a : ~a~%"            "Strike"    (dice-list-to-str (strike f)))
  (format t "~15@a : ~a~%"            "Defence"   (dice-list-to-str (defence f)))
  (format t "~a~%"                                (e-slot f)))

(defun print-grp-states ()
  (when (fellow *grp*)
    (format t "# ~8a      HP~%" "Name")
    (dolist (f (fellow *grp*))
      (format t "1 ~12a ~3d /~3d~%"
	      (name  f)
	      (hp    f)
	      (hpmax f)))))

;;;;====================================
;;;;
;;;; ITEM EQUIP & UNEQUIP

;;;; ex. (equip-from-bag fellow 5 (bag *grp*))


(defun equip-from-bag (fellow n grp)
  (when-let (i (nth n (bag grp))) ;バッグ内n番目のアイテムi
    (when-let (e (e-slot i))        ;iを装備するスロットe
      (when-let (remove-item
		 (assoc-value (e-slots fellow) (quote e))) ;fellowのスロットeの装備アイテム
	(put-item-in-bag remove-item *grp*)
	(equip-item fellow i)
	))))

(defun unequip-to-bag (fellow f-slot grp)
  (when-let (i (assoc-value (e-slots fellow) (quote f-slot)))
    (put-item-in-bag i grp)))
  
;;;;====================================
;;;;
;;;; GOLD

(defun pay-gold (g p)
  (if (> g (gold p))
      nil
      (decf (gold p) g)))

(defun get-gold (g p)
  (incf (gold p) g))

;;;;====================================
;;;;
;;;; COMBAT

(defun strike-by-player (player)
  (roll-dice-list (str player)))

(defun damage-to-player (player d)
  (let ((result (- (hp player) d)))
    (if (minusp result)
	(setf (hp player) 0)
	(setf (hp player) result))))

(defun heal-player (player h)
  (incf (hp player) h)
  (when (> (hp player) (hpmax player))
    (setf (hp player) (hpmax player))))

(defun dead-playerp (player)
  (if (<= (hp player) 0)
      t
      nil))

;;;;====================================
;;;;
;;;; BAG

(defun prompt-select-in-bag (prompt grp)
  (when (bag grp)
    (when-let (i
	       (prompt-number-between
		prompt 1 (length (bag grp)) t))
      i)))

;;;;====================================
;;;;
;;;; POROMT FOR PLAYER

(defun prompt-select-member (prompt grp)
  (if (single (fellow grp))
      (car (fellow grp))
      (let* ((n (length (fellow grp))))
	(when-let (i (prompt-number-between prompt 1 n t))
	  (nth (1- i) (fellow grp))
	  ))))

