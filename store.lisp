
;;;;====================================
;;;;
;;;; 売り物リスト

(defparameter *product-list*
  '(10 11 12 13 14 15
    100 101 102 103 104 105 106 107 108 109 110
    1000 1001 1002 1003))

;;;;====================================
;;;;
;;;; PRINT PRODUCT LIST

(defun print-product-list (pl item-table)
  ;;;;;;;;;;;;;;;;;;;;;;;書きかけ
  (let ((result nil)
	(i      nil))
    (dolist (n pl)
      (setf i (gethash n item-table))
      (setf result
	    (cons result
		  (fomrat nil "~16a ~5dG" (name i) (price i)))))
    (setf result (reverse result))
    (print-table-with-az result 23 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @store))
  (format t (c-str "   STORE   " :white :bg-black))
  (princ #\newline)
  (format t "いかがいたしましょうか？~%")
  (when-let (c (prompt-char-choice
		"P)urchase S)ell L)eave"
		'(#\P #\S #\L)))
    (case c
      (#\P (setf *process* @store-purchase@))
      (#\S (setf *process* @store-sell@))
      (#\L (progn
	     (format t "またのお越しを、お待ちしております。~%")
	     (setf *process* @castle@))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod game-process ((p @store-purchase))
  
  (print-product-list *product-list* *item-table*)

  (let ((n (length *product-list*)))
    (if-let (i (prompt-number-below "どれを購入しますか?" n))
      (let ((price (nth 4 (nth (1- i) *item-alist*)))       ;;;;; !
	    (name  (nth 0 (nth (1- i) *item-alist*))))      ;;;;; !
	(if (y-or-n-p
	     (format nil
		     " ~a ですね。~% ~a Gですが、お買いになりますか？"
		     name price))
	    (progn
	      (format t "御慧眼です!~%")
	      (if-let (balance (pay-gold price *grp*))
		(progn
		  (setf (gold *grp*) balance)
		  (put-new-item-in-bag name *grp*))
		(format t "...ですが、お金が足りないようです。~%")))
	    (progn
	      (format t "おや、残念です...~%")
	      (setf *process* @store@)))
	(setf *process* @store@)))))
  
(defmethod game-process ((p @store-sell))
  ())
