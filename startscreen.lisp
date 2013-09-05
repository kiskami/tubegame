;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; startscreen.lisp

(in-package #:tubegame)

(defparameter *STARTSCREEN-STATE* 'none)

(defun one-startscreen-frame (elapsedt)
  "Render one scrartscreen frame."
  (declare (ignore elapsedt)) ; for now
  (cond ((equal 'none *STARTSCREEN-STATE*)
	 (format t "Startscreen init...~%")
	 ; load startscreen assets
	 ; then run
	 (setq *STARTSCREEN-STATE* 'run))
	((equal 'run *STARTSCREEN-STATE*)
	 (format t "Startscreen run~%")
	 ; just switch to ingame mode for now
	 (setq *in-game* t))))
;  (if (< 0 elapsedt)
;      nil
;      nil))
