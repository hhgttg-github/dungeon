;;;;====================================
;;;;
;;;; GUILD VARIABLES

(defparameter +max-of-guild-member+ 26)
(defparameter +half-of-guild-member+ 13)

(defparameter *game-user-file-name* "game-user.data")

(defparameter *game-user-file-opened* nil)

(defparameter *game-user-file* nil)

(defparameter *guild* nil)

;;;;====================================
;;;;
;;;; INITIALIZE GUILD

(defun initialize-new-guild (gld)
  (setf gld (add-fellow-in-guild (:fighter gld)))
  (setf gld (add-fellow-in-guild (:cleric  gld)))
  (setf gld (add-fellow-in-guild (:thief   gld)))
  (setf gld (add-fellow-in-guild (:mage    gld))))

;;;;====================================
;;;;
;;;; GAME-USER FILE

(defclass game-user-data ()
  ((guild :initarg :guild :accessor guild :initform nil)
   (grp   :initarg :grp   :accessor grp   :initform nil)))

(defparameter *game-user-data* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-game-user-file (gld grp)
  (setf *game-user-data* (make-instance 'game-user-data
					:guild gld
					:grp   grp))
  (cl-store:store *game-user-data* *game-user-file-name*))

(defun open-game-user-file ()
  (when-let (f (cl-store:restore *game-user-file-name*))
    (setf *game-user-data* f)
    (setf *guild* (guild f))
    (setf *grp*   (grp   f))))

(defun close-game-user-file (gld grp)
  (setf (guild *game-user-data*) gld)
  (setf (grp   *game-user-data*) grp)
  (cl-store:store *game-user-data* *game-user-file-name*))

(defun new-game-user-file ()
  (if (y-or-n-p "新しいゲームファイルを作りますか?")
      (if (y-or-n-p "古いデータが破壊されますが、良いですか?")
	  (progn
	    (initialize-new-guild)
	    (create-game-user-file *guild* *grp*)))))

;;;;====================================
;;;;
;;;; ACCESS MEMBER OF GUILD

(defun guild-fellow-emptyp (gld)
  "満席ならnil"
  (when (>= (length gld)
	  +max-of-guild-member+)
    t))

(defun get-fellow-of-guild (n gld)
  (nth n gld))

(defun add-fellow-in-guild (f gld)
  "fを追加した(fellow gld)を返す。setfを忘れずに!"
  (append gld (list f)))

(defun remove-fellow-of-guild (n gld)
  "n番を取り除いた(fellow gld)を返す。setfを忘れずに!"
  (remove-nth n gld))

;;;;====================================
;;;; 
;;;; PRINT MEMBER

(defun print-guild-member-with-az (gld)
  (if gld
      (let* ((name-list nil)
	     (name-list1 nil)
	     (name-list2 nil)
	     (char-a (char-code #\A))
	     (char-n (+ char-a +half-of-guild-member+)))
       
       (dolist (i gld)
	 (setf name-list (cons (name i) name-list)))
       (setf name-list
	     (group (reverse name-list) +half-of-guild-member+))
       (setf name-list1 (car name-list))
       (setf name-list2 (cadr name-list))
       
       (dotimes (i +half-of-guild-member+)
	 (when-let (n (nth i name-list1))
	   (format t "  ~a ) ~20:a" (code-char char-a) n))
	 (when-let (n (nth i name-list2))
	   (format t "  ~a ) ~20:a"  (code-char char-n) n))
	 (princ #\newline)
	 (incf char-a)
	 (incf char-n))
       (princ #\newline))
      (format t "~% 現在、ギルドに所属するメンバーはいません。~%")))


;;;;====================================
;;;;
;;;; INSPECT, ADD, REMOVE

(defun inspect-guild-member (gld)
  (when-let (c (prompt-char-below "誰を確認しますか?"
				  (number-to-letter (length gld)) t))
    (print-fellow (nth (- c (char-code #\A)) gld))))

(defun new-guild-member (gld)
  (if (guild-fellow-emptyp gld)
      (progn())
      (format t "「申しわけありませんが、登録できる空き枠がありません。」~%")))

  
;;;;====================================
;;;;
;;;; GUILD PROCESS
  
(defmethod game-process ((p @guild))
  (format t (c-str "+=-=-=-=-=-=-=-=-=-=-=+~%" :white :bg-black))
  (format t (c-str "|      G U I L D      |~%" :white :bg-black))
  (format t (c-str "+=-=-=-=-=-=-=-=-=-=-=+~%" :white :bg-black))
  (print-guild-member-with-az *guild*)
  (when-let (c (prompt-char-choice
	      "I)nspect A)dd D)ismiss L)eave"
	      '(#\I #\A #\D #\L)))
    (case c
      (#\I (setf *process* @guild-inspect@))
      (#\A (setf *process* @guild-add@))
      (#\D (setf *process* @guild-dismiss@))
      (#\L (setf *process* @castle@)))))

;;;;====================================
;;;;
;;;; GUILD INSPECT

(defmethod game-process ((p @guild-inspect))
  (if (fellow *guild*)
      (inspect-guild-member *guild*)
      (format t "~% 登録されたメンバーが誰も居ません。~%~%")))

;;;;====================================
;;;;
;;;; GUILD ADD

(defmethod game-process ((p @guild-add))
  (format t " ギルドメンバー追加~%")
  (setf *process* @guild@))

;;;;====================================
;;;;
;;;; GUILD DISMISS

(defmethod game-process ((p @guild-dismiss))
  (format t " ギルドメンバー削除~%")
  (setf *process* @guild@))
  
