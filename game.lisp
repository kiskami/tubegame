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

(defconstant *COLLDET-DEBUGDRAWER-TIMEOUT* 2)
(defparameter *colldet-debugdrawer-time* 2)
(defparameter *colldet-debugdrawer-enabled* nil)

(defun toggle-debugdrawer (elapsedt)
  (setq *colldet-debugdrawer-time* (+ *colldet-debugdrawer-time* elapsedt))
  (if (<= *COLLDET-DEBUGDRAWER-TIMEOUT* *colldet-debugdrawer-time*)
      (progn 
	(setq *colldet-debugdrawer-time* 0)
	(setq *colldet-debugdrawer-enabled* (not *colldet-debugdrawer-enabled*))
	(llgs-engine-cl:colldet-enabledebugdrawer *colldet-debugdrawer-enabled*))))

(defun one-game-frame (elapsedt)
  "Update state and render one game playing frame."
  (cond ((equal *GAME-STATE* 'none)
	 ; first time or a new try?
	 (if (null *LEVEL1*) (setq *LEVEL1* (load-level1)) (reset-level1))
	 (if (null *PLAYER*) (setq *PLAYER* (load-player *LEVEL1*)) (reset-player *PLAYER* *LEVEL1*))

	 (show-level-and-player *LEVEL1* *PLAYER*)

	 (setq *GAME-STATE* 'loaded))
	(t ;running
	 ; ESC pressed?
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*)
	     ; end playing game
	     (end-game *LEVEL1*)
	   (if (< 0 elapsedt)
	       (progn
	       ; input
		 (if (llgs-engine-cl:input-keypressed *F12-KEY*)
		     (toggle-debugdrawer elapsedt))

	       ; move player
		 (if (llgs-engine-cl:input-keypressed *W-KEY*)
		     (player-forward *PLAYER* elapsedt))
		 (if (llgs-engine-cl:input-keypressed *S-KEY*)
		     (player-backward *PLAYER* elapsedt))

		 (let ((relx (llgs-engine-cl:input-mouserelx))
		       (rely (llgs-engine-cl:input-mouserely)))
		   (cond ((< 0 relx)
			  (player-rightturn *PLAYER* relx elapsedt))
			 ((> 0 relx)
			  (player-leftturn *PLAYER* relx elapsedt)))
		   (cond ((< 0 rely)
			  (player-downturn *PLAYER* rely elapsedt))
			 ((> 0 rely)
			  (player-upturn *PLAYER* rely elapsedt))))
	       ; fire
		 (if (llgs-engine-cl:input-leftmousebutton)
		     (player-fire *PLAYER* elapsedt))

	       ; perform colldet
		 (let ((collnum (llgs-engine-cl:colldet-perform)))
		   (if (< 0 collnum)
	       ; update entities (and player) on colldet
		       (format t "There are ~A collisions atm~%" collnum)))
	       
	       ; update entities (and player)

		 nil))))))

(defun load-level1 ()
  (format t "Loading level1...~%")
  (let ((level (make-leveldata)))
  (with-open-file (l *LEVEL1-FILE* :direction :input :if-does-not-exist :error)
    (setf (leveldata-levelfile level) (read l)))
  (setf (leveldata-levelpointmargin level) (first (third (leveldata-levelfile level))))
  (setf (leveldata-playerstructintegritystart level) (second (third (leveldata-levelfile level))))
  (setf (leveldata-playerweaponenergystart level) (third (third (leveldata-levelfile level))))
  (setf (leveldata-playershieldenergystart level) (fourth (third (leveldata-levelfile level))))
  level))

(defun end-game (level)
  (hide-level-and-player level)
  (setq *GAME-STATE* 'none)
  (setq *in-game* nil))

(defun show-level-and-player (level player)
  (show-level level)
  (show-player player))

(defun show-level (level)
  (declare (ignore level))
  (let ((mesh (llgs-engine-cl:mesh-load "asteroid1 mesh" *ASTEROID1-MESH*))
	(node (llgs-engine-cl:render-createscenenode "asteroid1")))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) node)
    (llgs-engine-cl:render-setscenenodepos node 2.0 2.0 2.0)
    (llgs-engine-cl:render-setscenenodescale node 2.0 2.0 2.0)
    ))

(defun hide-level (level)
  (declare (ignore level))
  nil)

(defun hide-level-and-player (level)
  (hide-player *PLAYER*)
  (hide-level level))

(defun reset-level1 ()
  nil)

