
;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage dungeon
  (:use :cl :asdf :alexandria))

(in-package dungeon)

(defsystem dungeon
  :name "dungeon"
  :version "0.0.1"
  :maintainer "ks"
  :author "ks"
  :serial t
  :components
  ((:file "variables")
   (:file "item"         :depends-on ("variables"))
   (:file "monster"      :depends-on ("variables"))
   (:file "maze"         :depends-on ("variables"
				     "item"
				     "monster"))
   (:file "player"       :depends-on ("variables"
				      "maze"))
   (:file "guild"        :depends-on ("variables"
				      "player"))
   (:file "initialize"   :depends-on ("variables"
			              "maze"))
   (:file "start-game"   :depends-on ("variables"))
   (:file "store"        :depends-on ("variables"))
   (:file "inn"          :depends-on ("variables"))
   (:file "castle"       :depends-on ("variables"))
   (:file "camp"         :depends-on ("variables"))
   (:file "battle"       :depends-on ("variables"))
   (:file "command"      :depends-on ("variables"
				      "maze"
				      "camp"
				      "battle"))
   (:file "main"         :depends-on ("variables"
				     "initialize"
				     "command"
				     "maze"))))
