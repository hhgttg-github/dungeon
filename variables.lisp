;;;;=======================================
;;;;
;;;; * * PROCESS * *

(defclass process ()
  ())

(defgeneric game-process (p))

;;;;====================================
;;;;
;;;; START-GAME, QUIT-GAME

(defclass @start-game (process)())
(defparameter @start-game@ (make-instance '@start-game))

(defclass @quit-game (process)())
(defparameter @quit-game@ (make-instance '@quit-game))

;;;;====================================
;;;;
;;;; CASTLE

(defclass @castle (process)())
(defparameter @castle@ (make-instance '@castle))

;;;;====================================
;;;;
;;;; GUILD

(defclass @guild         (process) ())
(defclass @guild-inspect (process) ())
(defclass @guild-add     (process) ())
(defclass @guild-dismiss (process) ())

(defparameter @guild@         (make-instance '@guild))
(defparameter @guild-inspect@ (make-instance '@guild-inspect))
(defparameter @guild-add@     (make-instance '@guild-add))
(defparameter @guild-dismiss@ (make-instance '@guild-dismiss))

;;;;====================================
;;;;
;;;; INN

(defclass @inn (process) ())
(defparameter @inn@ (make-instance '@inn))

;;;;====================================					
;;;;
;;;; STORE

(defclass @store (process)())
(defparameter @store@ (make-instance '@store))

(defclass @store-purchase (process)())
(defparameter @store-purchase@ (make-instance '@store-purchase))

(defclass @store-sell (process)())
(defparameter @store-sell@ (make-instance '@store-sell))

;;;;===================================
;;;;
;;;; MAIN-COMMAND

(defclass @before-command (process)())
(defparameter @before-command@ (make-instance '@before-command))

(defclass @main-command (process)())
(defparameter @main-command@ (make-instance '@main-command))

(defclass @after-command (process)())
(defparameter @after-command@ (make-instance '@after-command))

;;;;;===================================
;;;;
;;;; BATTLE

(defclass @before-battle (process)
  ((foe :initarg :foe :accessor foe)))
(defparameter @before-battle@
  (make-instance '@before-battle
		 :foe nil))

(defclass @battle-command (process)())
(defparameter @battle-command@ (make-instance '@battle-command))

(defclass @start-battle (process)())
(defparameter @start-battle@ (make-instance '@start-battle))

(defclass @defeat-monster (process)
  ((foe :initarg :foe :accessor foe)))
(defparameter @defeat-monster@ (make-instance '@defeat-monster :foe nil))

(defclass @game-over (process)())
(defparameter @game-over@ (make-instance '@game-over))

(defclass @run-from-battle (process)())
(defparameter @run-from-battle@ (make-instance '@run-from-battle))

;;;;====================================
;;;;
;;;; CAMP

(defclass @camp(process)())
(defparameter @camp@ (make-instance '@camp))

(defclass @camp-inspect(process)())
(defparameter @camp-inspect@ (make-instance '@camp-inspect))

(defclass @camp-spell(process)())
(defparameter @camp-spell@ (make-instance '@camp-spell))

(defclass @camp-bag(process)())
(defparameter @camp-bag@ (make-instance '@camp-bag))

(defclass @camp-bag-equip(process)())
(defparameter @camp-bag-equip@ (make-instance '@camp-bag-equip))

(defclass @camp-bag-use(process)())
(defparameter @camp-bag-use@ (make-instance '@camp-bag-use))

(defclass @camp-bag-drop(process)())
(defparameter @camp-bag-drop@ (make-instance '@camp-bag-drop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *process* nil)

(defparameter *quit-game* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; PLAYER VARIABLES

(defparameter *in-battle* nil)

(defparameter *print-grp-states* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MAP VARIABLES

(defparameter *map* nil) ;; (aref *map* index) -> tile class
(defparameter +map-portal+ nil)
(defparameter +num-of-message+ nil)
(defparameter *message* nil)
(defparameter *encount-table* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MONSTER VARIBALES

(defparameter *monster-alist* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; ITEM VARIABLES

(defparameter *item-alist* nil)


