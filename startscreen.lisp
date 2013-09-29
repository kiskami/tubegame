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
  (cond ((equal 'none *STARTSCREEN-STATE*)
	 (format t "Startscreen init...~%")
	 ; load startscreen assets
	 (llgs-engine-cl:render-createsimpletext "st_gamelabel" *GAMELABEL*
							 "DroidSans-Bold" 
							 48.0 10.0 10.0 1.0 1.0 1)
	 (llgs-engine-cl:render-createsimpletext "st_gamelabel2" *GAMELABEL2*
							 "DroidSans-Bold-14" 
							 14.0 10.0 60.0 1.0 1.0 1)
	 (llgs-engine-cl:render-simpletextcolor "st_gamelabel" 
						(first *LABELCOLOR*) 
						(second *LABELCOLOR*) 
						(third *LABELCOLOR*))
	 (llgs-engine-cl:render-createsimpletext "st_copylabel" *COPYRIGHT*
						 "DroidSans-14"
						 14.0 10.0 80.0 1.0 1.0 1)
	 (llgs-engine-cl:render-createsimpletext "st_pressany" *PRESS-ANY-KEY*
						 "DroidSans-Bold"
						 16.0 10.0 380.0 1.0 1.0 1)
	 (llgs-engine-cl:render-createsimpletext "st_controls" *CONTROLS*
						 "DroidSans"
						 14.0 10.0 350.0 1.0 1.0 1)
	 (llgs-engine-cl:render-createsimpleimage "si_3dlogo" "3DLogo"
						  0.0 0.83 0.105 0.1667 0)
	 ; then run
	 (setq *STARTSCREEN-STATE* 'run))
	((equal 'run *STARTSCREEN-STATE*)
;	 (format t "Startscreen run~%")
	 (cond ((< 0 (llgs-engine-cl:i-anykeypressed)) ; start playing?
		(setq *in-game* t)
		(llgs-engine-cl:render-simpletexthide "st_gamelabel")
		(llgs-engine-cl:render-simpletexthide "st_gamelabel2")
		(llgs-engine-cl:render-simpletexthide "st_copylabel")
		(llgs-engine-cl:render-simpletexthide "st_pressany")
		(llgs-engine-cl:render-simpletexthide "st_controls")
		(llgs-engine-cl:render-simpletexthide "si_3dlogo")
		)
	       (t
		(if (< 0 elapsedt)
		    ; update startscreen
		    (llgs-engine-cl:render-rotatescenenodey
		     *main-camera-node*
		     (adjust-float (* *STARTSCREEN-CAMROT-SPEED* elapsedt)))
		    ))))))
