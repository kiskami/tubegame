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
		  :leveldonepoints (leveldata-levelpointmargin level)
		  :integrity (leveldata-playerstructintegritystart level)
		  :startweaponenergy (leveldata-playerweaponenergystart level)
		  :weaponenergy (leveldata-playerweaponenergystart level)
		  :shieldenergy (leveldata-playershieldenergystart level)
		  :updatefunc #'update-player
		  :collfunc #'collision-player
		  :bouncetime 0
		  :bouncing nil
		  :bouncepenalty 0
		  :firetime *PLAYER-FIRE-TIMEOUT*
		  :firing nil
		  :movementdir 'forward
		  :relx 0
		  :rely 0
		  :bulletbillbnode (llgs-engine-cl:render-createchildscenenode
				    node
				    "player_billboardset" :inheritori 1 :inheritscale 0)
		  :bulletbillbset (llgs-engine-cl:billboardset-create)
		  :flymode nil
		  :playerrot 0
		  :speed *FLYSPEED*
		  :difficulty 0
		  :playtimer 0)))
    (llgs-engine-cl:render-attachmoveable pitchnode mesh)
    (llgs-engine-cl:render-attachmoveable (playerdata-bulletbillbnode plyent) (playerdata-bulletbillbset plyent))
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
    plyent))

(defun reset-player (player level)
  (setf (playerdata-levelpoints player) 0)
  (setf (playerdata-integrity player) (leveldata-playerstructintegritystart level))
  (setf (playerdata-weaponenergy player) (leveldata-playerweaponenergystart level))
  (setf (playerdata-shieldenergy player) (leveldata-playershieldenergystart level)))

(defun show-player (player camnode)
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

(defun go-forward (player elapsedt)
  (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* -1.0 (playerdata-speed player) elapsedt)) 0.0 t)
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun go-backward (player elapsedt)
  (llgs-engine-cl:render-translatescenenode (entitydata-node player) 0.0 (adjust-float (* (playerdata-speed player) elapsedt)) 0.0 t)
  (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player)))

(defun handle-turn (player elapsedt)
  (let ((rad (* -1.0 (playerdata-relx player) (adjust-float (* *TURNSPEED* elapsedt)))))
    (cond ((not (zerop (playerdata-relx player)))
	   (llgs-engine-cl:render-rotatescenenodez (entitydata-node player) rad)
	   (cond ((<= *PLAYER-MAX-ROT* (+ rad (playerdata-playerrot player)))
		  (setf rad (- *PLAYER-MAX-ROT* (playerdata-playerrot player)))
;		  (format t "handle-turn: rad1=~A~%" rad)
		  )
		 ((>= (* -1 *PLAYER-MAX-ROT*) (+ rad (playerdata-playerrot player)))
		  (setf rad (- (* -1 *PLAYER-MAX-ROT*) (playerdata-playerrot player)))
;		  (format t "handle-turn: rad2=~A~%" rad)
		  )))
	  (t
	   (when (not (zerop (playerdata-playerrot player))) ; rotate back ship after turns
	     (setf rad (* 
			(if (> (playerdata-playerrot player) 0) -1.0 1.0)
		        *PlAYER-BACK-ROT-SPEED*
			(adjust-float (* *TURNSPEED* elapsedt)))))))

    (incf (playerdata-playerrot player) rad)
    (llgs-engine-cl:render-rotatescenenodey (playerdata-pitchnode player) (adjust-float rad)))
  
  (if (not (zerop (playerdata-rely player)))
      (llgs-engine-cl:render-rotatescenenodex (entitydata-node player) (* (playerdata-rely player) (adjust-float (* *ROLLSPEED* elapsedt)))))

    (llgs-engine-cl:colldet-syncolobjtoscenenode (entitydata-physobj player) (entitydata-node player))
  )

(defun handle-flymode (player elapsedt)
  (cond ((playerdata-flymode player) ; freefly mode?
	 (cond ((eq (playerdata-movementdir player) 'forward)
		(go-forward player elapsedt))
	       ((eq (playerdata-movementdir player) 'backward)
		(go-backward player elapsedt)))
	 (setf (playerdata-movementdir player) nil)
	 (setf (playerdata-bouncetime player) 0))
	(t
	 (cond ((playerdata-bouncing player)
		(incf (playerdata-bouncetime player) elapsedt)

		(cond ((>= (playerdata-bouncetime player) *PLAYER-BOUNCE-TIMEOUT*)
;		       (format t "Bouncing penalty to player ~A~%" (playerdata-bouncepenalty player))
		       (setf (playerdata-bouncetime player) 0)
		       (decf (playerdata-integrity player) (playerdata-bouncepenalty player))
		       (when (>= 0 (playerdata-integrity player))
			 (setf (playerdata-integrity player) 0)
			 (game-over player))
		       (update-integrity-hud player))
;		(cond ((eq (playerdata-movementdir player) 'forward)
;		       (setf (playerdata-movementdir player) 'backward)
;;		       (go-backward player elapsedt)
;		       )
;		      ((eq (playerdata-movementdir player) 'backward)
;		       (setf (playerdata-movementdir player) 'forward)
;;		       (go-forward player elapsedt)))
		      ))
	       (t 
		(setf (playerdata-movementdir player) 'forward)
		(setf (playerdata-bouncetime player) 0)
		(go-forward player elapsedt)))))
  )

(defun handle-firing (player elapsedt)
  (cond ((playerdata-firing player)
	 (cond ((>= (+ (playerdata-firetime player) elapsedt) *PLAYER-FIRE-TIMEOUT*)
		(decf (playerdata-weaponenergy player) *BULLET-ENERGY*)
		(cond ((< 0 (playerdata-weaponenergy player))
;		       (format t "FIRE!~%")
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
  )

(defun update-player (player elapsedt)
  "Update player state according to input and collision events"

  (incf (playerdata-playtimer player) elapsedt)

  (handle-turn player elapsedt)

  (handle-flymode player elapsedt)

  (handle-firing player elapsedt)

  (raise-gamedifficulty player)

  (update-points-hud player)
  (setf (playerdata-relx player) 0)
  (setf (playerdata-rely player) 0)
  )

(defun raise-gamedifficulty (player)
  (when (and (= (playerdata-difficulty player) 0)
	   (>= (playerdata-levelpoints player) 
	       (* (playerdata-leveldonepoints player) 0.6)))
    (setf (playerdata-speed player) (* (playerdata-speed player) 1.2))
    (incf (playerdata-difficulty player))
    (format t "Raised game difficulty to ~A.~%" (playerdata-difficulty player)))
  (when (and (= (playerdata-difficulty player) 1)
	   (>= (playerdata-levelpoints player) 
	       (* (playerdata-leveldonepoints player) 0.8)))
    (setf (playerdata-speed player) (* (playerdata-speed player) 1.2))
    (incf (playerdata-difficulty player))
    (format t "Raised game difficulty to ~A.~%" (playerdata-difficulty player))))

(defun player-reset-collinfo (player)
    (setf (playerdata-bouncing player) nil))

(defun collision-player (player otherentity)
;  (format t "Player collided with entity type ~A~%" (entitydata-type otherentity))
  (cond ((eq 'asteroid (entitydata-type otherentity))
	 (player-bounce player otherentity *ASTEROID-BOUNCE-PENALTY*))
	((eq 'cube (entitydata-type otherentity))
	 (player-bounce player otherentity *CUBE-BOUNCE-PENALTY*)))
  ;TODO: powerups colldet!
  )

(defun player-bounce (player otherent penalty)
  (declare (ignore otherent))
  (setf (playerdata-bouncepenalty player) penalty)
  (setf (playerdata-bouncing player) t))

(defun player-rightturn (player relx)
  (setf (playerdata-relx player) relx))

(defun player-leftturn (player relx)
  (setf (playerdata-relx player) relx))

(defun player-downturn (player rely)
  (setf (playerdata-rely player) rely))

(defun player-upturn (player rely)
  (setf (playerdata-rely player) rely))

(defun player-fire (player elapsedt)
  (declare (ignore elapsedt))
;  (format t "Player fire~%")
  (setf (playerdata-firing player) t))

(defun player-forward (player)
  (when (playerdata-flymode player)
    (setf (playerdata-movementdir player) 'forward)))

(defun player-backward (player)
  (when (playerdata-flymode player)
    (setf (playerdata-movementdir player) 'backward)))

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
  (llgs-engine-cl:render-createsimpletext
   *RETICLEPANELID* "[ ]" "DroidSans" 0.04 0.485 0.482 1.0 1.0 0)

  (llgs-engine-cl:render-simpletextcolor *WEAPONPANELID* 
					 (first *GREEN-COLOR*)
					 (second *GREEN-COLOR*)
					 (third *GREEN-COLOR*))
  (llgs-engine-cl:render-simpletextcolor *INTEGPANELID*
 					 (first *GREEN-COLOR*)
					 (second *GREEN-COLOR*)
					 (third *GREEN-COLOR*))

  ; TODO shield, powerups
  )

(defun update-integrity-hud (player)
  (let ((swep (/ (playerdata-startweaponenergy player) 100)))
    (if (< (playerdata-weaponenergy player) (* 35 swep))
	(llgs-engine-cl:render-simpletextcolor *INTEGPANELID* 
					       (first *ORANGE-COLOR*)
					       (second *ORANGE-COLOR*)
					       (third *ORANGE-COLOR*)))
    (if (< (playerdata-weaponenergy player) (* 5 swep))
	(llgs-engine-cl:render-simpletextcolor *INTEGPANELID*
					       (first *ORANGE-COLOR*)
					       (second *ORANGE-COLOR*)
					       (third *ORANGE-COLOR*))))
  (llgs-engine-cl:render-simpletextsettext 
   *INTEGPANELID* 
   (make-hud-string "Ship integrity: " (playerdata-integrity player))))

(defun update-weapon-hud (player)
  (let ((swep (/ (playerdata-startweaponenergy player) 100)))
    (if (< (playerdata-weaponenergy player) (* 35 swep))
	(llgs-engine-cl:render-simpletextcolor *WEAPONPANELID*
					       (first *ORANGE-COLOR*)
					       (second *ORANGE-COLOR*)
					       (third *ORANGE-COLOR*)))
    (if (< (playerdata-weaponenergy player) (* 5 swep))
	(llgs-engine-cl:render-simpletextcolor *WEAPONPANELID*
					       (first *RED-COLOR*)
					       (second *RED-COLOR*)
					       (third *RED-COLOR*))))
  (llgs-engine-cl:render-simpletextsettext 
   *WEAPONPANELID*
   (make-hud-string "Weapon energy: " (playerdata-weaponenergy player))))

(defun update-points-hud (player)
  (llgs-engine-cl:render-simpletextsettext   
   *POINTSPANELID*
   (make-hud-string "Points: " (playerdata-levelpoints player))))

(defun hide-hud ())

(defun game-over (player)
;  (format t "*** GAME OVER ***~%")
  (llgs-engine-cl:render-createsimpletext "game_over" *GAME-OVER* 
					  "DroidSans-Bold"
					  0.10 0.2 0.4 1.0 1.0 0)
  (llgs-engine-cl:render-createsimpletext "st_pressany2" *PRESS-ANY-KEY2*
					  "DroidSans-Bold"
					  0.05 0.33 0.5 1.0 1.0 0)
  ; level done?
  (when (<= (playerdata-leveldonepoints player) (playerdata-levelpoints player))
    (llgs-engine-cl:render-createsimpletext "st_congrats" *CONGRAT-MSG*
					    "DroidSans-Bold"
					    0.10 0.25 0.56 1.0 1.0 0)
    (llgs-engine-cl:render-simpletextcolor "st_congrats" 0.851 0.644 0.125))
  (llgs-engine-cl:render-createsimpletext "st_destroyedmsg1"
					  (make-formatted-string *DESTROYED-POINTS-MSG1* 
								 (playerdata-levelpoints player) nil)
					  "DroidSans" 0.05 0.22 0.65 1.0 1.0 0)
  (let ((mins (floor (/ (playerdata-playtimer player) 60)))
	(secs (floor (mod (playerdata-playtimer player) 60))))
    (format t "Level done in ~A mins and ~A secs (~A).~%" mins secs
	    (playerdata-playtimer player))
    (llgs-engine-cl:render-createsimpletext "st_destroyedmsg2"
					    (make-formatted-string *DESTROYED-POINTS-MSG2* 
								   mins secs)
					    "DroidSans" 0.05 0.22 0.70 1.0 1.0 0))

  (setf *ENTITIES* nil)
  (setf *PHYSOBJMAP* (make-hash-table))
  (add-entity (make-explosiondata
	       :type 'game-over
	       :lifetime 0
	       :updatefunc #'game-over-update)))

(defun game-over-update (ent elapsedt)
  (incf (explosiondata-lifetime ent) elapsedt)
  (when (and (< *GAME-OVER-TIMEOUT* (explosiondata-lifetime ent)) (= 1 (llgs-engine-cl:i-anykeypressed)))
    (setf *game-should-exit* t)))

(defun player-toggle-flymode (player)
  (setf (playerdata-flymode player) (not (playerdata-flymode player)))
  (format t "Switched player flymode to ~A~%" (playerdata-flymode player)))
