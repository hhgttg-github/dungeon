;;;;====================================
;;;;
;;;; ITEM

(defclass item-effect ()
  ((operator :initarg :operator :accessor operator :initform nil)))

(defclass @nop (item-effect)
  ())

(defclass @healing-potion (item-effect)
  ())

;;;;
;;;; 使用例
;;;;
;;;; (defparamter *item-effect* '@healing-potion)
;;;; (defparameter @healing-potion@
;;;;               (make-instance *item-effect* :operator "2d6+6"))
;;;;

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
;;;; VARIABLES

(defparameter *item-table* nil)

(defparameter *item-key-table* (make-hash-table :test #'equal))

;;;;====================================
;;;;
;;;; FILE

(defparameter *item-data-file-name* #P"./game-item.data")

(defun write-item-data-file (it)
  (cl-store:store it *item-data-file-name*))

(defun read-item-data-file ()
  (cl-store:restore *item-data-file-name*))

;;;;====================================
;;;;
;;;; INITIALIZE ITEM

(defun initialize-item ()
  (format t "Initialize ITEM .... ")

  (setf *item-table* (read-item-data-file))

  (initialize-item-key-table *item-table*
			     *item-key-table*)
  
  (format t "DONE.~%"))

(defun initialize-item-key-table (it ikt)
  (let ((k (hash-table-keys   it)))
    (dolist (i k)
      (let ((n (key (gethash i it))))
	(setf (gethash n ikt) i)))))

;;;;====================================
;;;;
;;;; CREATE ITEM

(defun create-item (id)
  (let ((i (gethash id *item-table*)))
    (make-instance 'item-class
		   :id      (id      i)
		   :key     (key     i)
		   :name    (name    i)
		   :job     (job     i)
		   :e-slot  (e-slot  i)
		   :as      (as      i)
		   :assist  (assist  i)
		   :price   (price   i)
		   :effect  (effect  i)
		   :operand (operand i))))

(defun create-item-with-key (key)
  (create-item (gethash key *item-key-table*)))


;; (defun print-item-list (l)
;;   (format t "~15a ~5d~%" "Item" "Price")
;;   (dolist (i l)
;;     (format t "~15a ~5d~%" (car i) (nth 4 i))))

;; (defun print-item-list-# (l)
;;   (format t " ~2a ~15a ~5a~%" "#" "Item" "Price")
;;   (let ((n 1))
;;     (dolist (i l)
;;       (format t " ~2d ~15a ~5d~%" n (car i) (nth 4 i))
;;       (incf n))))

;;;;====================================
;;;;
;;;; +strike, -damage

(defun +strike (item-list)
  (let ((result 0))
    (dolist (i item-list)
      (when (or (eq (as i) :1h-w)
		(eq (as i) :2h-w))
	(incf result (roll-str (assist i)))))
    result))

(defun -damage (item-list)
    (let ((result 0))
      (dolist (i item-list)
	(when (or (eq (as i) :helm)
		  (eq (as i) :shield)
		  (eq (as i) :armor))
	  (incf result (roll-str (assist i)))))
      result))

;;;;====================================
;;;;
;;;; GET, NEW, DELETE

;;;;------------------------------------
;;;; BAGへの操作

;;;------------------------------------

(defun sort-bag (grp)
  (setf (bag grp) (sort (bag grp) #'< :key #'id)))

;;;------------------------------------

(defun put-item-in-bag (i grp)
  "すでに存在するアイテムをバッグに入れる"
  (setf (bag grp) (cons i (bag grp)))
  (sort-bag grp))

(defun get-nth-item-from-bag (n grp)
  (if-let (i (nth n (bag grp)))
    (progn
      (delete-nth-item-from-bag n grp)
      i)
    nil))

(defun put-new-item-in-bag (s grp)
  "文字列のアイテムを生成し、バッグに入れる"
  (setf (bag grp) (cons (create-item s) (bag grp)))
  (sort-bag grp))

(defun delete-item-from-bag (d-item grp)
  (setf (bag grp)
	(remove-if #'(lambda (i) (eql i d-item))
		   (bag grp)
		   :count 1))
  (sort-bag grp))

(defun delete-nth-item-from-bag (n grp)
  (if (nth n (bag grp))
      (progn
	(setf (bag grp) (remove-nth n (bag grp)))
	(sort-bag grp)
	(bag grp))
      nil))

;;;;------------------------------------
;;;; キャラクターへの操作

(defun remove-item-from-fellow (item-slot f)
  "スロットをnilにして、アイテムを返す。空きスロットならnil"
  (when-let (item (assoc-value (e-slots fellow) item-slot))
    item))

(defun check-equippable-on-fellow (i f)
  ;;;; 作成中
  "装備可能ならitemを、不可能ならnilを返す"
  (when f
    i))

(defun check-occupied-slot-of-item (i f)
  "アイテムiを装備するスロットは埋まっているか？空いてればnilを返す"
  (assoc-value (e-slots fellow) (e-slot i)))

(defun equip-item-on-fellow (i f)
  "装備できたときはそのitemを、出来なかったときはnilを返す"
(let ((e (e-slot i))))
)

;;;;====================================
;;;;
;;;; EQUIP & UNEQUIP ITEM

(defun remove-slot (s f)
  "指定されたスロットを外し、外したアイテムを返す"
  (when-let ((i (assoc-value (e-slots f) s)))
    (setf (assoc-value (e-slots f) s) nil)
    i))

(defun equip-item (item f)
  "スロットにアイテムを装備。既存のアイテムを事前に外しておくのを忘れないように!"
  (setf (assoc-value (e-slots f) (e-slot item)) item))

(defun equip-item-on-slot (item f)
  "スロットにアイテムを装備し、既装備アイテムを返す"
  (let ((i (remove-slot (e-slot item) f)))
    (equip-item item f)
    
    )

;;;;====================================
;;;;
;;;; USE ITEM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric use-item(i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass @healing-potion (item-effect) ())
(defparameter @healing-potion@ (make-instance '@healing-potion
					      :operator "2d6+6"))

(defmethod use-item ((i @healing-potion)))

;;;;====================================
;;;;
;;;; PRINT-bag

(defun print-bag-with-number(grp)
  (when (bag grp)
    (let ((result nil))
      (dolist (i (bag grp))
	(format t "~a~%" (name i))
	(setf result (cons (name i) result)))
      (print-table-with-number (reverse result) 3 10))))

(defun print-bag-with-az(grp)
  (when (bag grp)
    (let ((result nil))
      (dolist (i (bag grp))
	(format t "~a~%" (name i))
	(setf result (cons (name i) result)))
      (print-table-with-az (reverse result) 10 3))))

;;;;====================================
;;;;
;;;; PRINT EQUIPMENT

(defun equipment-string(fellow e)
  (if-let (i (assoc-value (e-slots fellow) e))
    (name i)
    ""
    ))

(defun print-equipment (fellow)
  (format t "~16@a : ~a~%" "Head"   (equipment-string fellow 'helm))
  (format t "~16@a : ~a~%" "Weapon" (equipment-string fellow 'melee))
  (format t "~16@a : ~a~%" "Shield" (equipment-string fellow 'shield))
  (format t "~16@a : ~a~%" "Armor"  (equipment-string fellow 'armor))
  (format t "~16@a : ~a~%" "Gloves" (equipment-string fellow 'gloves))
  (format t "~16@a : ~a~%" "Boots"  (equipment-string fellow 'boots)))


