;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; player.lisp

(in-package #:tubegame)

(defun load-player (level)
  (format t "Loading player data...~%")
  (let* ((mesh (llgs-engine-cl:mesh-load *PLAYER-MESH-NAME* *PLAYER-MESH*))
	 (node (llgs-engine-cl:render-createscenenode *PLAYER-NODE-NAME*))
	 (plyent (make-playerdata
		  :type 'player
		  :mesh mesh
		  :node node
		  :physobj (llgs-engine-cl:colldet-addmeshgeom (first *PLAYER-INITIAL-POS*)
					       (second *PLAYER-INITIAL-POS*) (third *PLAYER-INITIAL-POS*)
					       mesh 10.0 *PLAYER-PHYS-GRP* *PLAYER-PHYS-MASK*)
		  :levelpoints 0
		  :integrity (leveldata-playerstructintegritystart level)
		  :weaponenergy (leveldata-playerweaponenergystart level)
		  :shieldenergy (leveldata-playershieldenergystart level)
		  :updatefunc #'update-player
		  :collfunc #'collision-player)))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-setscenenodepos node (first *PLAYER-INITIAL-POS*)
					   (second *PLAYER-INITIAL-POS*) (third *PLAYER-INITIAL-POS*))
    (llgs-engine-cl:render-setscenenodescale node (first *PLAYER-NODE-SCALE*)
					     (second *PLAYER-NODE-SCALE*) (third *PLAYER-NODE-SCALE*))
    (llgs-engine-cl:render-rotatescenenodez node (adjust-float (deg-to-rad 90)))
    (llgs-engine-cl:render-rotatescenenodey node (adjust-float (deg-to-rad 90)))
    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj plyent) node)
    (llgs-engine-cl:colldet-setdynamic (entitydata-physobj plyent) 1)
    (llgs-engine-cl:colldet-setscale (entitydata-physobj plyent) (first *PLAYER-NODE-SCALE*)
				     (second *PLAYER-NODE-SCALE*) (third *PLAYER-NODE-SCALE*))
    plyent))

(defun reset-player (player level)
  (setf (playerdata-levelpoints player) 0)
  (setf (playerdata-integrity player) (leveldata-playerstructintegritystart level))
  (setf (playerdata-weaponenergy player) (leveldata-playerweaponenergystart level))
  (setf (playerdata-shieldenergy player) (leveldata-playershieldenergystart level)))

(defun show-player (player camnode)
;  (llgs-engine-cl:render-rotatescenenodez camnode (adjust-float (deg-to-rad 90)))
;  (llgs-engine-cl:render-rotatescenenodey camnode (adjust-float (deg-to-rad 90)))
  (llgs-engine-cl:render-setscenenodepos camnode 0.0 40.0 10.0)
  (llgs-engine-cl:render-addchild (entitydata-node player) camnode)
  (llgs-engine-cl:render-cameralookat *main-camera* (+ 2.0 (first *PLAYER-INITIAL-POS*))
					   (second *PLAYER-INITIAL-POS*) (third *PLAYER-INITIAL-POS*))
  (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) (entitydata-node player))
  (llgs-engine-cl:render-setscenenodevis (entitydata-node player) t)
  (add-entity player)
  (add-to-physobjmap (entitydata-physobj player) player))

(defun hide-player (player camnode)
  (llgs-engine-cl:render-setscenenodevis (entitydata-node player) nil)
  (llgs-engine-cl:render-removechild (entitydata-node player) camnode)
  (llgs-engine-cl:render-removechild (llgs-engine-cl:render-rootscenenode) (entitydata-node player))
  (remove-entity player)
  (del-from-physobjmap (entitydata-physobj player)))

(defun update-player (player elapsedt)
  nil)

(defun collision-player (player otherentity)
;  (format t "Player collided with entity: ~A type ~A~%" otherentity (entitydata-type otherentity))
  )

(defun player-rightturn (player relx elapsedt)
;  (llgs-engine-cl:render-rotatecameray *main-camera* (* relx (adjust-float (* *TURNSPEED* elapsedt)))))
  (llgs-engine-cl:render-rotatescenenodez (entitydata-node player) (* -1.0 relx (adjust-float (* *TURNSPEED* elapsedt))))
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun player-leftturn (player relx elapsedt)
;  (llgs-engine-cl:render-rotatecameray *main-camera* (* relx (adjust-float (* *TURNSPEED* elapsedt)))))
  (llgs-engine-cl:render-rotatescenenodez (entitydata-node player) (* -1.0 relx (adjust-float (* *TURNSPEED* elapsedt))))
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun player-downturn (player rely elapsedt)
;  (format t "Player down~%")
;  (llgs-engine-cl:render-rotatecamerax *main-camera* (* rely (adjust-float (* *ROLLSPEED* elapsedt)))))
  (llgs-engine-cl:render-rotatescenenodex (entitydata-node player) (* rely (adjust-float (* *ROLLSPEED* elapsedt))))
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun player-upturn (player rely elapsedt)
;  (format t "Player up~%")
;  (llgs-engine-cl:render-rotatecamerax *main-camera* (* rely (adjust-float (* *ROLLSPEED* elapsedt)))))
  (llgs-engine-cl:render-rotatescenenodex (entitydata-node player) (* rely (adjust-float (* *ROLLSPEED* elapsedt))))
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun player-fire (player elapsedt)
  (format t "Player fire~%"))

(defun player-forward (player elapsedt)
;  (format t "Player forward~%")
  (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* -1.0 *FLYSPEED* elapsedt)) 0.0 t)
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))
;  (llgs-engine-cl:render-movecameraforward *main-camera* (adjust-float (* *FLYSPEED* elapsedt))))

(defun player-backward (player elapsedt)
;  (format t "Player backward~%")
  (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* *FLYSPEED* elapsedt)) 0.0 t)
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))
;  (llgs-engine-cl:render-movecameraforward *main-camera* (* -1 (adjust-float (* *FLYSPEED* elapsedt)))))
