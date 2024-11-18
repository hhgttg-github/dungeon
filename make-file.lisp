;;; 必要なファイルは３つ
;;; 1) make-file.lisp
;;; 2) program-name.asd
;;; 3) main.lisp

; 2) program-name.asdには、パッケージの設定と使用ファイル一覧が記載されている。その中に"Main"が含まれる。
; 3) プログラム本体
; REPLで(load "make-file.lisp")
; その後、(main) で実行
;; (require 'asdf)
;; (asdf:operate 'asdf:load-op 'tiny-sdl)  ; "program-name.asd"を読み込んで、パッケージを設定する。

(require 'asdf)
(asdf:operate 'asdf:load-op 'dungeon) 
