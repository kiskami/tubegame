;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; game.lisp

(in-package #:tubegame)

(defparameter *GAME-STATE* 'none)
(defparameter *LEVEL* nil)
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
	 (if (not *LEVEL*) (setq *LEVEL* (load-level1)) (reset-level *LEVEL*))
	 (if (not *PLAYER*) (setq *PLAYER* (load-player *LEVEL*)) (reset-player *PLAYER* *LEVEL*))

	 (show-level-and-player *LEVEL* *PLAYER*)

	 (setq *GAME-STATE* 'loaded))
	(t ;running
	 ; ESC pressed?
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*)
	     ; end playing game
	     (end-game *LEVEL*)
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
  "Read in level data file and create leveldata structure with initial values."
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

(defun dimX (level)
  (first (first level)))

(defun dimY (level)
  (second (first level)))

(defun dimZ (level)
  (third (first level)))

(defun show-level (level)
  "Process level data and create entities and scene objects."
  (let ((l (leveldata-levelfile level)))
  (loop for y from 0 to (1- (dimY l)) do 
       (leveldata-yplane y (dimZ l) (dimX l) (nth y (second l))))))

(defun leveldata-yplane (y dimz dimx ypl)
  (loop for z from 0 to (1- dimz) do
       (leveldata-row y z dimx (nth z ypl))))

(defun leveldata-row (y z dimx row)
  (loop for x from 0 to (1- dimx) do
       (apply #'apply-rowdata x y z (nth x row))))

(defun apply-rowdata (x y z &optional cube &key (rotx 0) (roty 0) (rotz 0)
		      sta cel
		      (ast1 0) (ast2 0) (ast3 0) 
		      (ss 0) (pa 0) (wea 0) (spe 0) 
		      (tur 0) (ene 0))
		      "Create game entities within 1 cube and add them to scene."
  (if cube
      (progn
	(create-cube x y z cube rotx roty rotz)
	(if sta (create-startpoint x y z))
	(if cel (create-endpoint x y z))
	(if (> ast1 0) (create-asteroid x y z ast1 1))
	(if (> ast2 0) (create-asteroid x y z ast2 2))
	(if (> ast3 0) (create-asteroid x y z ast3 3))
	(if (> ss 0) (create-strinteg-powerup x y z ss))
	(if (> pa 0) (create-shield-powerup x y z pa))
	(if (> wea 0) (create-weapon-powerup x y z wea))
	(if (> spe 0) (create-speed-powerup x y z spe))
	(if (> tur 0) (create-turret x y z tur))
	(if (> ene 0) (create-enemy x y z ene)))))

(defun hide-level (level)
  (declare (ignore level))
  nil)

(defun hide-level-and-player (level)
  (hide-player *PLAYER*)
  (hide-level level))

(defun reset-level (level)
  (declare (ignore level))
  nil)

(defun create-cube (x y z cube rotx roty rotz)
  "Create cube mesh and scenenode, position it and rotate around its local x, then y, then z axis."
 (let ((mesh (llgs-engine-cl:mesh-load (gen-name "mesh" cube x y z) (get-meshfile-name cube)))
	(node (llgs-engine-cl:render-createscenenode (gen-name "cube" x y z))))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) node)
    (multiple-value-call #'llgs-engine-cl:render-setscenenodepos node (calc-cube-pos (adjust-float x) (adjust-float y) (adjust-float z)))
;    (llgs-engine-cl:render-setscenenodescale node 0.5 0.5 0.5)
    (if (and rotx (> rotx 0)) (llgs-engine-cl:render-rotatescenenodex node (adjust-float (deg-to-rad rotx))))
    (if (and roty (> roty 0)) (llgs-engine-cl:render-rotatescenenodey node (adjust-float (deg-to-rad roty))))
    (if (and rotz (> rotz 0)) (llgs-engine-cl:render-rotatescenenodez node (adjust-float (deg-to-rad rotz))))
;; TODO: create and add physobj
    ))

(defun gen-name (base &rest rest)
  (let ((namestr (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
  (with-output-to-string (s namestr)
    (format s "~A_~{~A_~}" base rest)
    namestr)))

(defun get-meshfile-name (cube)
  (cond ((= cube 1) *CUBE1-MESH*)
	((= cube 2) *CUBE2-MESH*)
	((= cube -2) *CUBE-2-MESH*)
	((= cube 3) *CUBE3-MESH*)
	((= cube -3) *CUBE-3-MESH*)
	((= cube 4) *CUBE4-MESH*)
	((= cube -4) *CUBE-4-MESH*)
	((= cube 5) *CUBE5-MESH*)
	((= cube 6) *CUBE6-MESH*)
	(t *CUBE1-MESH*)))

(defun calc-cube-pos (x y z)
  (values (* 2 x) (* 2 y) (* 2 z)))

(defun create-startpoint (x y z)
  (declare (ignore x y z))
  nil)

(defun create-endpoint (x y z)
  (declare (ignore x y z))
  nil)

(defun create-asteroid (x y z type n)
  (declare (ignore x y z type n))
  nil)

(defun create-strinteg-powerup (x y z ss)
  (declare (ignore x y z ss))
  nil)

(defun create-shield-powerup (x y z pa)
  (declare (ignore x y z pa))
  nil)

(defun create-weapon-powerup (x y z wea)
  (declare (ignore x y z wea))
  nil)

(defun create-speed-powerup (x y z spe)
  (declare (ignore x y z spe))
  nil)

(defun create-turret (x y z tur)
  (declare (ignore x y z tur))
  nil)

(defun create-enemy (x y z ene)
  (declare (ignore x y z ene))
  nil)
