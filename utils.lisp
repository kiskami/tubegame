;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; utils.lisp

(in-package #:tubegame)

(defun adjust-float (f)
  "Truncate floats to 5 decimal fraction."
  (/ (truncate (* f 100000.0)) 100000.0))

(defun deg-to-rad (deg)
  (* deg *ONEDEGREE*))

(defun add-to-physobjmap (ptr entity)
  (setf (gethash ptr *PHYSOBJMAP*) entity))

(defun get-from-physobjmap (ptr)
  (gethash ptr *PHYSOBJMAP*))

(defun del-from-physobjmap (ptr)
  (remhash ptr *PHYSOBJMAP*))
