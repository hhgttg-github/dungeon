
;;;;====================================
;;;;
;;;; ITEM

(defclass item-class ()
  ((id       :initarg :id       :accessor id      :initform 0)
   (key      :initarg :key      :accessor key     :initform "nameless")
   (name     :initarg :name     :accessor name    :initform "名前無しアイテム")
   (job      :initarg :job      :accessor job     :initform #b0000)
   (e-slot   :initarg :e-slot   :accessor e-slot  :initform nil
	     :documentation
	    "nil helm melee shield armor gloves boots")
   (as       :initarg :as       :accessor as      :initform nil
	     :documentation
	    "nil melee1 melee2 armor potion scroll")
   (assist   :initarg :assist   :accessor assist  :initform "0d0+0")
   (price    :initarg :price    :accessor price   :initform 0)
   (effect   :initarg :effect   :accessor effect  :initform nil)
   (operand  :initarg :operand  :accessor operand :initform nil)))

;;;;====================================
;;;;
;;;; PRINT ITEM

(defun print-item (i)
  (format t "~8@a:~a/~16a ~16a~%"
	  "ID" (id i) (key i) (name i))
  (format t "~8@a:~4b     Price:~aG~%"
	  "Job" (job i) (price i))
  (format t "~8@a:~a as ~a~%"
	  "Slot" (e-slot i) (as i))
  (format t "~8@a:~a~%"
	  "Power"  (dice-to-str (assist i)))
  (format t "~8@a:~a / ~a~%"
	  "Effect" (effect i) (operand i)))

;;;;====================================
;;;;
;;;; FILE

(defparameter *item-csv-file-name*  #P"./game-item.csv")

(defparameter *item-data-file-name* #P"../game-item.data")

(defun write-item-data-file ()
  (cl-store:store *item-table*
		  *item-data-file-name*))

(defun read-item-data-file ()
  (cl-store:restore *item-data-file-name*))

;;;;====================================
;;;;
;;;; ITEM DATA HASH-TABLE

(defparameter *item-table* (make-hash-table :test #'equal))

;;;;====================================
;;;;
;;;; ITEM-CSV -> ITEM.DATA

(defun convert-item-csv-to-data-file ()
  (let ((csv (cdr (cl-csv:read-csv *item-csv-file-name*)))) ; 1行目は項目欄なのでcdr
    (dolist (i csv)
      (let ((item (make-instance 'item-class
				 :id      (parse-integer (nth 0 i))
				 :key     (nth 1 i) 
				 :name    (nth 2 i)
				 :job     (read-from-string (nth 3 i))
				 :e-slot  (read-from-string (nth 4 i))
				 :as      (read-from-string (nth 5 i))
				 :assist  (str-to-dice (nth 6 i))
				 :price   (parse-integer (nth 7 i))
				 :effect  (read-from-string (nth 8 i))
				 :operand (nth 9 i))))
	(print-item item)
	(setf (gethash (id item) *item-table*) item)))))

;;;;====================================
;;;;
;;;; MAKE ITEM DATA FILE

(defun make-item-data-file ()
  (convert-item-csv-to-data-file)
  (format t "CONVERT ITEM CSV -- DONE.~%")
  (write-item-data-file))
