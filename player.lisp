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
	 (pitchnode (llgs-engine-cl:render-createchildscenenode node *PLAYER-PITCHNODE-NAME*))
	 (plyent (make-playerdata
		  :type 'player
		  :mesh mesh
		  :node node
		  :pitchnode pitchnode
		  :physobj (llgs-engine-cl:colldet-addmeshgeom (first *PLAYER-INITIAL-POS*)
					       (second *PLAYER-INITIAL-POS*) (third *PLAYER-INITIAL-POS*)
					       mesh *PLAYER-PHYS-GRP* *PLAYER-PHYS-MASK*)
		  :levelpoints 0
		  :integrity (leveldata-playerstructintegritystart level)
		  :weaponenergy (leveldata-playerweaponenergystart level)
		  :shieldenergy (leveldata-playershieldenergystart level)
		  :updatefunc #'update-player
		  :collfunc #'collision-player
		  :bouncetimer (llgs-engine-cl:timer-create)
		  :bouncing nil
		  :firetime *PLAYER-FIRE-TIMEOUT*
		  :firing nil
		  :movementdir nil
		  :relx 0
		  :rely 0
		  :bulletbillbnode (llgs-engine-cl:render-createchildscenenode
				    node
				    "player_billboardset" :inheritori 1 :inheritscale 0)
		  :bulletbillbset (llgs-engine-cl:billboardset-create)
		  :flymode nil)))
    (llgs-engine-cl:render-attachmoveable pitchnode mesh)
    (let ((startpos (first (leveldata-startposlist level))))
      (if (not startpos)
	  (setf startpos *PLAYER-INITIAL-POS*))
      (llgs-engine-cl:render-setscenenodepos node (first startpos) (second startpos) (third startpos)))
    (llgs-engine-cl:render-setscenenodescale pitchnode (first *PLAYER-NODE-SCALE*)
					     (second *PLAYER-NODE-SCALE*) (third *PLAYER-NODE-SCALE*))
    (llgs-engine-cl:render-rotatescenenodez node (adjust-float (deg-to-rad 90)))
    (llgs-engine-cl:render-rotatescenenodey node (adjust-float (deg-to-rad 90)))
    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj plyent) node)
    (llgs-engine-cl:colldet-setscale (entitydata-physobj plyent) (first *PLAYER-NODE-SCALE*)
				     (second *PLAYER-NODE-SCALE*) (third *PLAYER-NODE-SCALE*))
    (llgs-engine-cl:billboardset-setdefdims (playerdata-bulletbillbset plyent) *PLAYER-BULLET-W* *PLAYER-BULLET-H*)
    (llgs-engine-cl:billboardset-setmaterial (playerdata-bulletbillbset plyent) *PLAYER-BULLET-MAT*)
    (llgs-engine-cl:render-attachmoveable (playerdata-bulletbillbnode plyent) (playerdata-bulletbillbset plyent))
    plyent))

(defun reset-player (player level)
  (setf (playerdata-levelpoints player) 0)
  (setf (playerdata-integrity player) (leveldata-playerstructintegritystart level))
  (setf (playerdata-weaponenergy player) (leveldata-playerweaponenergystart level))
  (setf (playerdata-shieldenergy player) (leveldata-playershieldenergystart level)))

(defun show-player (player camnode)
;  (llgs-engine-cl:render-rotatescenenodez camnode (adjust-float (deg-to-rad 90)))
;  (llgs-engine-cl:render-rotatescenenodey camnode (adjust-float (deg-to-rad 90)))
  (llgs-engine-cl:render-setscenenodepos camnode 0.0 0.3 0.1)
  (llgs-engine-cl:render-addchild (entitydata-node player) camnode)
  (llgs-engine-cl:render-cameralookat *main-camera* (+ 2.0 (first *PLAYER-INITIAL-POS*))
					   (second *PLAYER-INITIAL-POS*) (third *PLAYER-INITIAL-POS*))
  (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) (entitydata-node player))
  (llgs-engine-cl:render-setscenenodevis (entitydata-node player) t)
  (add-entity player)
  (add-to-physobjmap (entitydata-physobj player) player)
  (show-hud player))

(defun hide-player (player camnode)
  (llgs-engine-cl:render-setscenenodevis (entitydata-node player) nil)
  (llgs-engine-cl:render-removechild (entitydata-node player) camnode)
  (llgs-engine-cl:render-removechild (llgs-engine-cl:render-rootscenenode) (entitydata-node player))
  (remove-entity player)
  (del-from-physobjmap (entitydata-physobj player))
  (hide-hud))

(defun update-player (player elapsedt)
  "Update player state according to input and collision events"
  (when (not (= 0 (playerdata-relx player)))
    (let ((rad (* -1.0 (playerdata-relx player) (adjust-float (* *TURNSPEED* elapsedt)))))
      (llgs-engine-cl:render-rotatescenenodez (entitydata-node player) rad)
      (llgs-engine-cl:render-rotatescenenodey (playerdata-pitchnode player) rad)
      (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player))))

  (cond ((>= 0 (playerdata-rely player))
	 (llgs-engine-cl:render-rotatescenenodex (entitydata-node player) 
						 (* (playerdata-rely player) (adjust-float (* *ROLLSPEED* elapsedt))))
	 (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))
	((< 0 (playerdata-rely player))
	 (llgs-engine-cl:render-rotatescenenodex (entitydata-node player) 
						 (* (playerdata-rely player) (adjust-float (* *ROLLSPEED* elapsedt))))
	 (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player))))

  (when (or (not (playerdata-bouncing player)) (playerdata-flymode player))
    (cond ((eq (playerdata-movementdir player) 'forward)
	   (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* -1.0 *FLYSPEED* elapsedt)) 0.0 t)
	   (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))
	  ((eq (playerdata-movementdir player) 'backward)
	   (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* *FLYSPEED* elapsedt)) 0.0 t)
	   (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))))

  (cond ((playerdata-firing player)
	 (cond ((>= (+ (playerdata-firetime player) elapsedt) *PLAYER-FIRE-TIMEOUT*)
		(decf (playerdata-weaponenergy player) *BULLET-ENERGY*)
		(cond ((< 0 (playerdata-weaponenergy player))
		       (format t "FIRE!~%")
		       (let ((pos (llgs-engine-cl:render-getscenenodepos (playerdata-node player))))
			 (add-entity (apply #'create-bullet 'playerbullet 
					    (list (playerdata-bulletbillbset player) 
						  (first pos) (second pos) (third pos)
						  (playerdata-node player) player)))))
		      (t 
		       (setf (playerdata-weaponenergy player) 0)))
		(update-weapon-hud player)
		(setf (playerdata-firetime player) 0))
	       (t
		(incf (playerdata-firetime player) elapsedt)))
	 (setf (playerdata-firing player) nil))
	(t
	 (incf (playerdata-firetime player) elapsedt)))

  (update-points-hud player)
  (setf (playerdata-relx player) 0)
  (setf (playerdata-rely player) 0)
  (setf (playerdata-movementdir player) nil)
  (setf (playerdata-bouncing player) nil)
)

(defun collision-player (player otherentity)
;  (format t "Player collided with entity type ~A~%" (entitydata-type otherentity))
  (cond ((eq 'asteroid (entitydata-type otherentity)) (player-bounce player otherentity *ASTEROID-BOUNCE-PENALTY*))
	((eq 'cube (entitydata-type otherentity)) (player-bounce player otherentity *CUBE-BOUNCE-PENALTY*)))
  ;TODO: cel, powerups colldet!
  )

(defun player-bounce (player otherent penalty)
  (decf (playerdata-integrity player) penalty)
  (when (>= 0 (playerdata-integrity player))
      (setf (playerdata-integrity player) 0)
      (game-over player))
  (setf (playerdata-bouncing player) t)
  (update-integrity-hud player))

(defun player-rightturn (player relx)
  (setf (playerdata-relx player) relx))

(defun player-leftturn (player relx)
  (setf (playerdata-relx player) relx))

(defun player-downturn (player rely)
  (setf (playerdata-rely player) rely))

(defun player-upturn (player rely)
  (setf (playerdata-rely player) rely))

(defun player-fire (player elapsedt)
;  (format t "Player fire~%")
  (setf (playerdata-firing player) t))

(defun player-forward (player)
  (setf (playerdata-movementdir player) 'forward))

(defun player-backward (player)
  (setf (playerdata-movementdir player) 'backward))

(defun show-hud (player)
  ;points
  (llgs-engine-cl:render-createsimpletext 
   *POINTSPANELID*
   (make-hud-string "Points: " (playerdata-levelpoints player))
   "DroidSans" 32.0 10.0 10.0 1.0 1.0 1)
  ;integrity
  (llgs-engine-cl:render-createsimpletext 
   *INTEGPANELID*
   (make-hud-string "Ship integrity: " (playerdata-integrity player))
   "DroidSans" 24.0 10.0 45.0 1.0 1.0 1)
  ;weapon energy
  (llgs-engine-cl:render-createsimpletext 
   *WEAPONPANELID*
   (make-hud-string "Weapon energy: " (playerdata-weaponenergy player))
   "DroidSans" 24.0 10.0 71.0 1.0 1.0 1)
  ; TODO shield, powerups
  )

(defun update-integrity-hud (player)
  (llgs-engine-cl:render-simpletextsettext 
   *INTEGPANELID* 
   (make-hud-string "Ship integrity: " (playerdata-integrity player))))

(defun update-weapon-hud (player)
  (llgs-engine-cl:render-simpletextsettext 
   *WEAPONPANELID*
   (make-hud-string "Weapon energy: " (playerdata-weaponenergy player))))

(defun update-points-hud (player)
  (llgs-engine-cl:render-simpletextsettext   
   *POINTSPANELID*
   (make-hud-string "Points: " (playerdata-levelpoints player))))

(defun hide-hud ())

(defun game-over (player)
  (format t "*** GAME OVER ***~%"))

(defun player-toggle-flymode (player)
  (setf (playerdata-flymode player) (not (playerdata-flymode player))))
