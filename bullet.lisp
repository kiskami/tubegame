;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; bullet.lisp

(in-package #:tubegame)

(defun create-bullet (type billbset x y z)
  (make-bulletdata
   :type type
   :physobj nil ; TODO create physobj
   :updatefunc #'bullet-update
   :collfunc #'bullet-collfunc
   :energy *BULLET-ENERGY*
   :lifetime 0
   :flydist 0
   :billset billbset
   ; TODO align to player ship nose :)
   :billboard (llgs-engine-cl:billboard-create billbset (+ x 1) y z 1.0 1.0 1.0)))

(defun blow-bullet (bullet)
  (llgs-engine-cl:billboard-remove (bulletdata-billset bullet) (bulletdata-billboard bullet))
  (remove-entity bullet)
  (add-entity (create-bullet-explosion bullet)))

(defun bullet-update (bullet elapsedt)
  (incf (bulletdata-lifetime bullet) elapsedt)
  (if (> (bulletdata-lifetime bullet) *BULLET-LIFETIME*)
      (blow-bullet bullet)
      ; TODO bullet fly
      ))

(defun bullet-collfunc (bullet otherobj)
  (declare (ignore bullet otherobj)))

(defun create-bullet-explosion (bullet)
  nil)
