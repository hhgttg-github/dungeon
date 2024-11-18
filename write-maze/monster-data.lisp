
;;;;====================================
;;;;
;;;; MONSTER

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
;;;; MONSTER & ENCOUNT HASH-TABLE

(defparameter *monster-table* (make-hash-table))

;;(defparameter *monster-key-table* (make-hash-table :test #'equal))

(defparameter *encount-table* (make-hash-table))

;;;;====================================
;;;;
;;;; FILE

(defparameter *monster-csv-file-name* #P"/home/kawabe/MEGA/lisp/dungeon/write-maze/game-monster.csv")

(defparameter *encount-csv-file-name* #P"/home/kawabe/MEGA/lisp/dungeon/write-maze/game-encount.csv")

(defparameter *monster-data-file-name* #P"../game-monster.data")

(defun read-monster-data-file ()
  (let ((data (cl-store:restore *monster-data-file-name*)))
    (setf *monster-table* (car data))
    (setf *encount-table* (cadr data))))

(defun write-monster-data-file ()
  (cl-store:store (list *monster-table* *encount-table*)
		  *monster-data-file-name*))

;;;;====================================
;;;;
;;;; MONSTER.CSV -> MONSTER HASH TABLE
;;;; ENCOUNT.CSV -> ENCOUT HASH TABlE

(defun make-act-list (l)
  (format t "make-act-list~%")
  (let ((result nil)
	(act (group (last l 16) 2)))
    (dolist (i act)
      (setf result
	    (cons (list (parse-integer    (car i))
			(read-from-string (cadr i)))
		  result)))
    (format t "result = ~a~%" result)
    (reverse result)))

(defun convert-monster-csv-to-table ()
  (let ((csv
	  (cdr (cl-csv:read-csv *monster-csv-file-name*)))) ; 1行目は項目欄なのでcdr
    (dolist (i csv)
      (let ((monster (make-instance 'monster-class
				    :id       (parse-integer (nth 0 i))
				    :key      (nth 1 i) 
				    :name     (nth 2 i)
				    :dice     (str-to-dice (nth 3 i))
				    :hpmax    (parse-integer (nth 4 i))
				    :ex       (parse-integer (nth 5 i))
				    :gold     (parse-integer (nth 6 i))
				    :item%    (parse-integer (nth 7 i))
				    :item     (read-from-string (nth 8 i))
				    :act      (make-act-list i)))) ; act-listは必ず16必要 ->(group (last 16 i) 2)
	(print-monster monster)
	(setf (gethash (id monster) *monster-table*) monster)))))

(defun convert-encount-csv-to-table ()
  (let ((csv
	  (cdr (cl-csv:read-csv *encount-csv-file-name*)))) ; 1行目は項目欄なのでcdr
    (print csv)
    (princ #\newline)
    (dolist (i csv)
      (setf i (remove-if #'(lambda (n) (string= n "")) i))
      (let ((id (parse-integer (car i)))
	    (l  (group (cdr i) 2))
	    (result nil))
	(dolist (j l)
	  (setf result
		(cons (list (parse-integer    (car j))
			    (read-from-string (cadr j)))
		      result)))
	(setf (gethash id *encount-table*) (reverse result))))))

;;;;====================================
;;;;
;;;; MAKE MONSTER DATA FILE

(defun make-monster-data-file ()
  (convert-monster-csv-to-table)
  (format t "CONVERT MONSTER CSV -- DONE.~%")
  (convert-encount-csv-to-table)
  (format t "CONVERT ENCOUNT CSV -- DONE.~%")
  (write-monster-data-file))


