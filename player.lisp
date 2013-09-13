;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; player.lisp

(in-package #:tubegame)

(defconstant *TURNSPEED* (* *ONEDEGREE* 14))
(defconstant *ROLLSPEED* (* *ONEDEGREE* 12))
(defconstant *FLYSPEED* 1)

(defun load-player (level)
  (format t "Loading player data...~%")
  (let ((mesh (llgs-engine-cl:mesh-load *PLAYER-MESH-NAME* *PLAYER-MESH*))
	(node (llgs-engine-cl:render-createscenenode *PLAYER-NODE-NAME*)))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-setscenenodepos node 0.0 0.0 0.0)
    (llgs-engine-cl:render-setscenenodescale node 0.025 0.025 0.025)
    (llgs-engine-cl:render-rotatescenenodez node (adjust-float (deg-to-rad 90)))
    (llgs-engine-cl:render-rotatescenenodey node (adjust-float (deg-to-rad 90)))
    (make-playerdata
     :mesh mesh
     :node node
     :levelpoints 0
     :integrity (leveldata-playerstructintegritystart level)
     :weaponenergy (leveldata-playerweaponenergystart level)
     :shieldenergy (leveldata-playershieldenergystart level))))

(defun reset-player (player level)
  (setf (playerdata-levelpoints player) 0)
  (setf (playerdata-integrity player) (leveldata-playerstructintegritystart level))
  (setf (playerdata-weaponenergy player) (leveldata-playerweaponenergystart level))
  (setf (playerdata-shieldenergy player) (leveldata-playershieldenergystart level)))

(defun show-player (player)
  (llgs-engine-cl:render-addchild 
   (llgs-engine-cl:render-rootscenenode)
   (entitydata-node player)))

(defun hide-player (player)
  (llgs-engine-cl:render-removechild
   (llgs-engine-cl:render-rootscenenode)
   (entitydata-node player)))

(defun player-rightturn (player relx elapsedt)
;  (format t "Player right~%")
  (llgs-engine-cl:render-rotatecameray *main-camera* (* relx (adjust-float (* *TURNSPEED* elapsedt)))))

(defun player-leftturn (player relx elapsedt)
;  (format t "Player left~%")
  (llgs-engine-cl:render-rotatecameray *main-camera* (* relx (adjust-float (* *TURNSPEED* elapsedt)))))

(defun player-downturn (player rely elapsedt)
;  (format t "Player down~%")
  (llgs-engine-cl:render-rotatecamerax *main-camera* (* rely (adjust-float (* *ROLLSPEED* elapsedt)))))

(defun player-upturn (player rely elapsedt)
;  (format t "Player up~%")
  (llgs-engine-cl:render-rotatecamerax *main-camera* (* rely (adjust-float (* *ROLLSPEED* elapsedt)))))

(defun player-fire (player elapsedt)
  (format t "Player fire~%"))

(defun player-forward (player elapsedt)
;  (format t "Player forward~%")
  (llgs-engine-cl:render-movecameraforward *main-camera* (adjust-float (* *FLYSPEED* elapsedt))))

(defun player-backward (player elapsedt)
;  (format t "Player backward~%")
  (llgs-engine-cl:render-movecameraforward *main-camera* (* -1 (adjust-float (* *FLYSPEED* elapsedt)))))
