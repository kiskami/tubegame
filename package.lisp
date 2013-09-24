;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; package.lisp

(in-package :cl-user)

(defpackage #:tubegame
  (:use #:cl #:llgs-engine-cl)
  (:export :game-run))
