
;;;;====================================
;;;;
;;;; FILES

(defparameter *monster-data-file-name* #P"./game-monster.data")

(defun read-monster-data-file ()
  (cl-store:restore *monster-data-file-name*))

;;;;====================================
;;;;
;;;; VARIABLES

(defparameter *monster-table* nil)

(defparameter *monster-key-table* (make-hash-table :test #'equal))

(defparameter *encount-table* (make-hash-table :test #'equal))

;;;;====================================
;;;;
;;;; INITIALIZE MONSTER

(defun initialize-monster ()
  (format t "Initialize MONSTER .... ")

  (let ((monster-data-file (read-monster-data-file)))
    (setf *monster-table* (car  monster-data-file))
    (setf *encount-table* (cadr monster-data-file)))

  (initialize-monster-key-table *monster-table*
				*monster-key-table*)
  
  (format t "DONE~%"))

(defun initialize-monster-key-table (mt mkt)
  (let ((k (hash-table-keys mt)))
    (dolist (i k)
      (let ((n (key (gethash i mt))))
	(setf (gethash n mkt) i)))))

;;;;====================================
;;;;
;;;; CLASS

(defclass monster-class ()
  ((id    :initarg :id    :accessor id    :initform nil)
   (key   :initarg :key   :accessor key   :initform "")
   (name  :initarg :name  :accessor name  :initform "")
   (dice  :initarg :dice  :accessor dice  :initform nil)
   (hpmax :initarg :hpmax :accessor hpmax :initform 1)
   (ex    :initarg :ex    :accessor ex    :initform 0)
   (gold  :initarg :gold  :accessor gold  :initform 0)
   (item% :initarg :item% :accessor item% :initform 0)
   (item  :initarg :item  :accessor item  :initform nil)
   (act   :initarg :act   :accessor act   :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass monster-runtime-class (monster-class)
  ((hp     :initarg :hp     :accessor hp     :initform 1)
   (states :initarg :states :accessor states :initform nil)
   (drop   :initarg :drop   :accessor dorp   :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass monster-grp-class ()
  ((fellow :initarg :fellow :accessor fellow :initform nil)))

(defparameter *monster-grp*
  (make-instance 'monster-grp-class
		 :fellow nil))

;;;;====================================
;;;;
;;;; PRINT MONSTER

(defun print-monster (m)
  (format t "~8@a:~a/~16a ~16a~%"
	  "ID" (id m) (key m) (name m))
  (format t "~8@a:~4b     HP:~a~%"
	  "Dice" (dice-to-str (dice m)) (hpmax m))
  (format t "~8@a:~a ~8@a:~a~%"
	  "Exp" (ex m) "Gold" (gold m))
  (format t "~8@a:~a(~a%)~%"
	  "Item"  (item m) (item% m)))

;;;;====================================
;;;;
;;;; CREATE MONSTER

(defun create-monster (monster)
  (if-let (i (assoc monster *monster-alist* :test #'string=))
    (progn
      (let ((hp (roll-str (nth 1 i))))
	(make-instance 'monster-class
		       :name  monster
		       :dice  (nth 1 i)
		       :hp    hp
		       :hpmax hp)))
    (error "指定されたモンスターは存在しません。")))

(defun create-monster-grp (m-grp)
  "((モンスター1 数)(モンスター2 数)(..)..) のリストからパーティーを作る。
   パーティーはフラットなリストになる。(m1 m1 m2 m2 m3 m4..)"
  (when m-grp
    (let ((result nil))
      (dolist (m-group m-grp)
	(dotimes (i (cadr m-group))
	  (setf result
		(cons (create-monster (car m-group))
		      result))))
      (reverse result))))

;;;;====================================
;;;;
;;;; ENCOUNT
 
(defun roll-encount (table id)
  "encount-tableから、登場するモンスターを決める。"
  (if-let (tbl (nth id table))
    (let ((dice-result (roll-ndpz 1 (car tbl) 0)))
      (loop for i in (cdr tbl) do
	(if (<= dice-result (cadr i))
	    (return (car i))
	    (decf dice-result (cadr i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encountp (m p e)
  "m:map p:grp e:encount-table エンカウントが発生する?
   発生するなら、該当モンスターを返す"
  (let* ((pos  (pos p))
	 (e%   (encount%   (aref m pos)))
	 (e-id (encount-id (aref m pos))))
    (when-let (tbl (nth e-id e))
      (when (roll-100 e%)
        (roll-encount e e-id)))))

;;;;====================================
;;;;
;;;; COMBAT

(defun strike-by-monster (m)
  "m : monster"
  (roll-str (dice m)))

(defun damage-to-monster (m d)
  (decf (hp m) d))

(defun heal-monster (m h)
  (incf (hp m) h)
  (when (> (hp m) (hpmax m))
    (setf (hp m) (hpmax m))))

(defun dead-monsterp (m)
  (if (<= (hp m) 0)
      t
      nil))
