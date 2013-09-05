;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; game.lisp

(in-package #:tubegame)

(defparameter *GAME-STATE* 'none)
(defparameter *LEVEL1* nil)
(defparameter *PLAYER* nil)

(defun one-game-frame (elapsedt)
  "Update state and render one game playing frame."
  (cond ((equal *GAME-STATE* 'none)
	 (if (null *PLAYER*) (setq *PLAYER* (load-player)))
	 (if (null *LEVEL1*) (setq *LEVEL1* (load-level1)))
	 (show-level-and-player)
	 (setq *GAME-STATE* 'loaded))
	(t ;running
	 ; ESC pressed?
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*)
	     ; end playing game
	     (end-game)
	   (if (< 0 elapsedt)
	       nil)))))

(defun load-level1 ()
  (format t "Loading level1...~%")
  (with-open-file (l *LEVEL1-FILE* :direction :input :if-does-not-exist :error)
    (setq *LEVEL1* (read l))))

(defun load-player ()
  (format t "Loading player data...~%")
  nil)

(defun end-game ()
  (hide-level-and-player)
  (setq *GAME-STATE* 'none)
  (setq *in-game* nil))

(defun show-level-and-player ()
  nil)

(defun hide-level-and-player ()
  nil)
