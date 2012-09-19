; Definition file

(defpackage :tic-tac-toe
  (:use :cl :asdf))

(in-package :tic-tac-toe)

(defsystem :tic-tac-toe
  :name "Tic-tac-toe"
  :author "Robert Berry <rjberry@gmail.com>"
  :version "0.2"
  :maintainer "Robert Berry <rjberry@gmail.com>"
  :description "Example minimax algorithm for the game."
  :depends-on (:alexandria)
  :licence "GPL 3.0"
  :components ((:file "utils")
               (:file "move")
               (:file "board")
               (:file "move-heuristics")
               (:file "state")
               (:file "state-heuristics")
               (:file "minimax")
               (:file "tic-tac-toe")))
