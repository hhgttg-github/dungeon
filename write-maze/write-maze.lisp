;;;;====================================
;;;;
;;;; CONSTANT

(defparameter +map-width+ 20)
(defparameter +map-height+ 20)
(defparameter +map-size+ (* +map-width+ +map-height+))
(defparameter +map-portal+ 361)
(defparameter *map-portal* nil)

;;;;====================================
;;;;
;;;; TILE SECTION

(defclass tile ()
  ((terrain    :accessor terrain    :initarg :terrain)
   (passable   :accessor passable   :initarg :passable :initform t)
   (chip       :accessor chip       :initarg :chip)
   (message    :accessor message    :initarg :message)
   (encount%   :accessor encount%   :initarg :encount%)
   (encount-id :accessor encount-id :initarg :encount-id)))

(defparameter +tile-table-size+ 100)
(defparameter *tile-table*
  (make-array +tile-table-size+ :initial-element nil))

(defun set-tile-table (id terrain passable 2ch fg bg message e% e-id)
  (setf (aref *tile-table* id)
	(list terrain passable (c-str 2ch fg bg) message e% e-id)))

(set-tile-table  0 :path t   "  " :black :bg-cyan 0 5 1)
(set-tile-table  1 :wall nil "  " :white :bg-blue 0 0 0)
(set-tile-table  2 :door t   "[]" :black :bg-magenda 0 50 2)
(set-tile-table 11 :path t   "  " :black :bg-cyan 1 5 1)
(set-tile-table 21 :path t   ".!" :black :bg-cyan 2 5 1)
(set-tile-table 31 :path t   "?." :black :bg-cyan 3 5 1)
(set-tile-table  9 :door t   "<>" :red   :bg-green 4 100 3)

(defun set-with-tile-table (m i d)
  (let ((tile (aref *tile-table* d)))
    (with-slots (terrain passable chip message encount% encount-id) (aref m i)
      (setf terrain    (nth 0 tile))
      (setf passable   (nth 1 tile))
      (setf chip       (nth 2 tile))
      (setf message    (nth 3 tile))
      (setf encount%   (nth 4 tile))
      (setf encount-id (nth 5 tile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MAP DATA SECTION

(defparameter *map*
  (aops:generate (lambda ()
		   (make-instance 'tile
				  :terrain  :path
				  :passable t
				  :chip     (c-str "//" :red :bg-white)
				  :message  nil
				  :encount% 0
				  :encount-id nil))

		 +map-size+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *message*
  '(nil
    "1 これはメッセージです。"
    "2 これは2行にまたがるメッセージです。
インデントがズレるのが玉に傷ですね。"
    "3 The quick brown fox jumps over the lazy dog."
    "4 ドラゴンがいる場所だ。"))

(defparameter +num-of-message+ (length *message*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *map-data*
  '( 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
     1  0  0  1  0  0  1  0  0  0  1  0  0  0  0  0  0  1  0  1 
     1  0  0  2  0  0  1  2  1  2  1  2  1  1  1  1  0  1  0  1 
     1  2  1  1  1  2  0  0  1  0  0  0  0  0  0  1  2  1  2  1 
     1  0  0  0  1  1  0  0  1  0  0  0  0  0  0  1  0  0  0  1 
     1  1  1  2  1  1  2  1  1  0  0  0  0  0  0  1  0  0  0  1 
     1  0  2  0  0  1  0  0  1  1  1  1  1  1  1  1  1  0  0  1 
     1  0  1  0  0  2  0  0  0  1  0  0  0  1  0  0  1  2  1  1 
     1  0  1  1  1  1  1  1  1  1  1  1  2  1  2  1  1  0  0  1
     1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1
     1  0  1  1  1  1  1  1  1  1  2  1  1  0  1  1  1  1  0  1 
     1  0  1  0  0  0  0  0  0  0  0  0  1  0  1  0  1  0  1  1 
     1  0  1  0  0  1  0  1  1  0  1  0  1  0  0  0  0  0  0  1 
     1  9  1  0  1  0  0  0  1  0  1  0  1  0  1  1  1  1  0  1 
     1  0  1  0  0  0  1  0  0  0  0  0  1  0  0  0  1  0  0  1 
     1 31  1  0  1  0  0  1  0  1  0  1  1  0  1  1  1  1  0  1 
     1 21  2  0  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  1 
     1 11  1  1  1  1  1  1  1  1  1  1  1  0  1  1  1  1  0  1 
     1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1
     1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun data-to-map ()
  (let ((d nil))
    (dotimes (i +map-size+)
      (setf d (nth i *map-data*))
      (set-with-tile-table *map* i d))))

(defun draw-map ()
  (let ((c 0))
    (dotimes (i +map-size+)
      (format t (chip (aref *map* i)))
      (incf c)
      (when (= c +map-width+)
	(princ #\newline)
	(setf c 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MONSTER SECTION

(defparameter *encount-table* '(nil
				;;;----- 1
				(nil            ; 重みの合計
				 ("slime" 10)
				 ("kobold" 5)
				 ("skelton" 2)
				 ("lv2 rogue" 1))
				;;;----- 2
				(nil
				 ("skelton" 10)
				 ("lv2 rogue" 3)
				 ("dragon"  1))
				;;;----- 3
				(nil
				 ("dragon" 1))))

(defun sum-encount-weight (table)
  "重み合計を求める。テーブルが変わるたびに再計算"
  (let ((total 0))
    (dolist (i table)
      (when i
	(incf total (cadr i))))
    total))

(defun make-encount-table (table)
  (dolist (i table)
    (when i
      (setf (car i) (sum-encount-weight i)))))

;;;;====================================
;;;;
;;;; FILE SECTION

(defparameter *game-data-path* #p"/home/kawabe/MEGA/lisp/dungeon/game.data")

(defun store-game-file ()

  (make-encount-table *encount-table*)
  
  (cl-store:store (list +map-portal+
			*map*
			+num-of-message+
			*message*
			*monster-alist*
			*encount-table*)
		  *game-data-path*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun restore-game-file ()
  (let ((m (cl-store:restore *game-data-path*)))
    (setf *map-portal*     (car m))
    (setf *map*            (cadr m))
    (setf +num-of-message+ (nth 2 m))
    (setf *message*        (nth 3 m))
;    (setf *item-alist*     (nth 4 m))
    (setf *monster-alist*  (nth 4 m))
    (setf *encount-table*  (nth 5 m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-game-file ()
  (data-to-map)
  (store-game-file))
