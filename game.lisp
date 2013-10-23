;;;; tubegame
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;;
;;;; game.lisp

(in-package #:tubegame)

(defparameter *GAME-STATE* 'none)
(defparameter *LEVEL* nil)
(defparameter *PLAYER* nil)

(defparameter *colldet-debugdrawer-time* 1.5)
(defparameter *colldet-debugdrawer-enabled* nil)

(defparameter *colldet-time* 0)

(defparameter *flymode-switch-time* 1.5)

(defun toggle-debugdrawer ()
  (when (<= *COLLDET-DEBUGDRAWER-TIMEOUT* *colldet-debugdrawer-time*)
    (setq *colldet-debugdrawer-time* 0)
    (setq *colldet-debugdrawer-enabled* (not *colldet-debugdrawer-enabled*))
    (format t "Setting physics debugdraw to ~A~%" *colldet-debugdrawer-enabled*)
    (llgs-engine-cl:colldet-setdebugdrawmode (if *colldet-debugdrawer-enabled* 10 0))))

(defun toggle-flymode (player)
  (when (<= *FLYMODE-SWITCH-TIMEOUT* *flymode-switch-time*)
    (setq *flymode-switch-time* 0)
;    (format t "Switching fly mode.~%")
    (player-toggle-flymode player)))

(defun one-game-frame (elapsedt)
  "Update state and render one game playing frame."
  (cond ((equal *GAME-STATE* 'none)
	 ; first time or a new try?
	 (setf *ENTITIES* nil)
	 (setf *PHYSOBJMAP* (make-hash-table))

	 (if (not *LEVEL*) (setq *LEVEL* (load-level1)) (reset-level *LEVEL*))
	 (if (not *PLAYER*) (setq *PLAYER* (load-player *LEVEL*)) (reset-player *PLAYER* *LEVEL*))
	 (if (not *EXPBILLBSET*) (init-explosions (playerdata-node *PLAYER*)) (reset-explosions))

	 (show-level-and-player *LEVEL* *PLAYER*)

	 (setq *GAME-STATE* 'loaded)
	 )
	(t ;running
	 ; ESC pressed?
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*)
	     ; end playing game
	     (end-game *LEVEL*)
	   (when (< 0 elapsedt)
	       ; input
	     (incf *colldet-debugdrawer-time* elapsedt)
	     (if (llgs-engine-cl:input-keypressed *F12-KEY*)
		 (toggle-debugdrawer))
	     (incf *flymode-switch-time* elapsedt)
	     (if (llgs-engine-cl:input-keypressed *F10-KEY*)
		 (toggle-flymode *PLAYER*))

	     ; move player
	     (if (llgs-engine-cl:input-keypressed *W-KEY*)
		 (player-forward *PLAYER*))
	     (if (llgs-engine-cl:input-keypressed *S-KEY*)
		 (player-backward *PLAYER*))
	     
	     (let ((relx (llgs-engine-cl:input-mouserelx))
		   (rely (llgs-engine-cl:input-mouserely)))
	       (cond ((< 0 relx)
		      (player-rightturn *PLAYER* relx))
		     ((>= 0 relx)
		      (player-leftturn *PLAYER* relx)))
	       (cond ((< 0 rely)
		      (player-downturn *PLAYER* rely))
		     ((>= 0 rely)
		      (player-upturn *PLAYER* rely))))
     	     ; fire
	     (if (llgs-engine-cl:input-leftmousebutton)
		 (player-fire *PLAYER* elapsedt))
	     
	     (incf *colldet-time* elapsedt)
	     ; perform colldet and entity update
	     (when (>= *colldet-time* *COLLDET-TIMEOUT*)
	       (setf *colldet-time* 0)
	       (player-reset-collinfo *PLAYER*)
	       (let ((collnum (llgs-engine-cl:colldet-perform)))
		 (when (< 0 collnum)
;	       (format t "There are ~A collisions atm.~%" collnum)
	     ; update entities (and player) on colldet
		   (dotimes (i collnum) 
		     (let* ((collpair (llgs-engine-cl:colldet-getcollpair i))
			    (entA (get-from-physobjmap (car collpair)))
			    (entB (get-from-physobjmap (cdr collpair))))
		       (if entA (funcall (entitydata-collfunc entA) entA entB))
		       (if entB (funcall (entitydata-collfunc entB) entB entA)))))))

	       (clean-colldet-trash)

	       ; update entities (and player)
	       (map nil #'(lambda (e) 
			    (funcall (entitydata-updatefunc e) e elapsedt))
		    *ENTITIES*)
	     )))))

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
  (show-player player *main-camera-node* level))

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
       (leveldata-yplane level y (dimZ l) (dimX l) (nth y (second l))))))

(defun leveldata-yplane (level y dimz dimx ypl)
  (loop for z from 0 to (1- dimz) do
       (leveldata-row level y z dimx (nth z ypl))))

(defun leveldata-row (level y z dimx row)
  (loop for x from 0 to (1- dimx) do
;       (format t "leveldata-row: ~A ~A ~A ~A~%" x y z (nth x row))
       (apply #'apply-rowdata level x y z (nth x row))))

(defun apply-rowdata (level x y z &optional cube &key (rotx 0) (roty 0) (rotz 0)
		      sta cel
		      (ast1 0) (ast2 0) (ast3 0) 
		      (ss 0) (pa 0) (wea 0) (spe 0) 
		      (tur 0) (ene 0))
		      "Create game entities within 1 cube and add them to scene."
  (when cube
	(add-entity (create-cube x y z cube rotx roty rotz))
	(when sta 
	  (add-entity (create-startpoint x y z))
	  (let ((pos (calc-cube-pos (adjust-float x) (adjust-float y) (adjust-float z))))
	    (pushnew pos (leveldata-startposlist level))
	    (format t "Registered startpoint at ~A~%" pos)))
	(if cel (add-entity (create-endpoint x y z)))
	(if (> ast1 0) (add-entity (create-asteroid x y z 'ast1 ast1)))
	(if (> ast2 0) (add-entity (create-asteroid x y z 'ast2 ast2)))
	(if (> ast3 0) (add-entity (create-asteroid x y z 'ast3 ast3)))
	(if (> ss 0) (add-entity (create-strinteg-powerup x y z ss)))
	(if (> pa 0) (add-entity (create-shield-powerup x y z pa)))
	(if (> wea 0) (add-entity (create-weapon-powerup x y z wea)))
	(if (> spe 0) (add-entity (create-speed-powerup x y z spe)))
	(if (> tur 0) (add-entity (create-turret x y z tur)))
	(if (> ene 0) (add-entity (create-enemy x y z ene)))))

(defun hide-level (level)
  (declare (ignore level))
  nil)

(defun hide-level-and-player (level)
  (hide-player *PLAYER* *main-camera-node*)
  (hide-level level))

(defun reset-level (level)
  (declare (ignore level))
  nil)

(defun create-cube (x y z cube rotx roty rotz)
  "Create cube mesh and scenenode, position it and rotate around its local x, then y, then z axis."
;  (format t "Creating cube ~A at(~A,~A,~A), rot(~A,~A,~A)~%" cube x y z rotx roty rotz)
  (let* ((pos (calc-cube-pos (adjust-float x) (adjust-float y) (adjust-float z)))
	 (mesh (llgs-engine-cl:mesh-load (gen-name "cube" cube x y z) (get-cube-meshfile-name cube)))
	 (node (llgs-engine-cl:render-createscenenode (gen-name "cube" x y z)))
	 (cubeent (make-entitydata 
		 :type 'cube
		 :mesh mesh
		 :node node
		 :physobj (llgs-engine-cl:colldet-addmeshgeom (first pos) (second pos) (third pos) 
							      mesh *CUBE-PHYS-GRP* *CUBE-PHYS-MASK*)
		 :updatefunc #'update-null
		 :collfunc #'colldet-null)))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) node)
    (llgs-engine-cl:render-setscenenodepos node (first pos) (second pos) (third pos))
;    (llgs-engine-cl:render-setscenenodescale node 0.5 0.5 0.5)
    (if (and rotx (> rotx 0)) (llgs-engine-cl:render-rotatescenenodex node (adjust-float (deg-to-rad rotx))))
    (if (and roty (> roty 0)) (llgs-engine-cl:render-rotatescenenodey node (adjust-float (deg-to-rad roty))))
    (if (and rotz (> rotz 0)) (llgs-engine-cl:render-rotatescenenodez node (adjust-float (deg-to-rad rotz))))
    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj cubeent) (entitydata-node cubeent))
    (add-to-physobjmap (entitydata-physobj cubeent) cubeent)
    cubeent))

(defun update-null (e elapsedt)
  "No op update function for cubes."
  (declare (ignore e elapsedt)))

(defun colldet-null (cube otherobj)
  "No op colldet function for cubes."
  (declare (ignore cube otherobj)))

(defun get-cube-meshfile-name (cube)
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
  (list (* 2 x) (* 2 y) (* 2 z)))

(defun create-startpoint (x y z)
  (declare (ignore x y z))
  nil)

(defun create-endpoint (x y z)
  (let* ((pos (calc-cube-pos (adjust-float x) (adjust-float y) (adjust-float z)))
	(mesh (llgs-engine-cl:mesh-load (gen-name "cel" x y z) *CEL-MESH*))
	(node (llgs-engine-cl:render-createscenenode (gen-name "cel" x y z)))
	(celent (make-entitydata
		 :type 'cel
		 :mesh mesh
		 :node node
		 :physobj (llgs-engine-cl:colldet-addcylinder (first pos) 
							      (second pos)
							      (third pos)
							      *CELCIL-HALFEXT1*
							      *CELCIL-HALFEXT2*
							      *CELCIL-HALFEXT3*
							      *CEL-PHYS-GRP*
							      *CEL-PHYS-MASK*)
		 :updatefunc #'cel-update
		 :collfunc #'cel-colldet)))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) node)
    (llgs-engine-cl:render-setscenenodepos node (first pos) (second pos) (third pos))
    (llgs-engine-cl:render-setscenenodescale node 
					     (first *CELNODE-SCALE*)
					     (second *CELNODE-SCALE*)
					     (third *CELNODE-SCALE*))
    (llgs-engine-cl:render-rotatescenenodez node (adjust-float (/ pi 2)))
    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj celent) (entitydata-node celent))
    (add-to-physobjmap (entitydata-physobj celent) celent)
    celent))

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

(defun cel-colldet (cel otherobj)
  (declare (ignore cel))
;  (format t "cel-colldet: collision with ~A~%" otherobj)
  (game-over otherobj))

(defun cel-update (e elapsedt)
  (when (< 0 elapsedt)
    (llgs-engine-cl:render-rotatescenenodey (entitydata-node e) 
					    (* *CEL-ROT-SPEED* elapsedt))
;    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj e) 
;						 (entitydata-node e))
    ))
