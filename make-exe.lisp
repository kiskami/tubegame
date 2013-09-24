;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; make-exe.lisp

(unless (find-package :asdf)
         (require :asdf))

;;; Saving Executables

(require "tubegame")

(defparameter name #+unix "tubegame" #+windows "tubegame.exe")

;;; Only Clozure CL (on Windows) is supported atm
#+ccl (save-application name :toplevel-function #'tubegame:game-run :prepend-kernel t)
