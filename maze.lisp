
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +map-width+  20)
(defparameter +map-size+ (* +map-width+ +map-width+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 以下の関数は、縦横ともに+maze-widht+の正方形のマップでのみ有効。

(defun to-1d (x y)
  (+ x (* +map-width+ y)))

(defun to-2d (i)
  (reverse
   (multiple-value-list
    (floor i +map-width+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun over-edge (i dx dy)
"上下/左右を繋げる。ただし、縦横ともに+maze-width
+の正方形のマップのみ有効。"
  (let* ((xy (to-2d i))
	 (x (+ (car xy) dx))
	 (y (+ (cadr xy) dy)))
    (when (minusp x)
      (setf x (+ +map-width+ x)))
    (when (minusp y)
      (setf y (+ +map-width+ y)))
    (when (>= x +map-width+)
      (setf x (- x +map-width+)))
    (when (>= y +map-width+)
      (setf y (- y +map-width+)))
    (to-1d x y)))
    
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

(defun restore-game-file ()
  (let ((m (cl-store:restore "game.data")))
    (setf +map-portal+     (car m))
    (setf *map*            (cadr m))
    (setf +num-of-message+ (nth 2 m))
    (setf *message*        (nth 3 m))
    (setf *item-alist*     (nth 4 m))
    (setf *monster-alist*  (nth 5 m))
    (setf *encount-table*  (nth 6 m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-map ()
  (let ((c 0))
    (dotimes (i +map-size+)
      (format t (chip (aref *map* i)))
      (incf c)
      (when (= c +map-width+)
	(princ #\newline)
	(setf c 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-3x3 (i)
  (let ((s nil)
	(dx '(-1 0 1))
	(dy '(-1 0 1)))
    (dolist (y dy)
      (dolist (x dx)
	(setf s (concatenate 'string s (chip (aref *map* (over-edge i x y))))))
      (setf s (concatenate 'string s "~%")))
    (format t s)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-message (i)
  (if-let (m (nth (message (aref *map* i)) *message*))
    (format t m))
  (princ #\newline))

