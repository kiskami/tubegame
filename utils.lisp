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

(defun del-physobj (ptr)
  (pushnew ptr *PHYSOBJMAP-TRASH*))

(defun clean-colldet-trash ()
  (dolist (o *PHYSOBJMAP-TRASH*)
    (llgs-engine-cl:colldet-delcolobj o))
  (setf *PHYSOBJMAP-TRASH* nil))

(defun add-entity (ent)
  (when ent (pushnew ent *ENTITIES*)))

(defun remove-entity (ent)
  (when ent (setf *ENTITIES* (delete ent *ENTITIES*))))

(defun gen-name (base &rest rest)
  (let ((namestr (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
  (with-output-to-string (s namestr)
    (format s "~A_~{~A_~}" (gensym base) rest)
    namestr)))

(defun get-rnd-coord (max)
  (let ((r (random (* max 200))))
    (adjust-float (/ (- r (* max 100)) 100.0))))
