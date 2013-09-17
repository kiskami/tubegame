;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; tubegame.lisp

(in-package :cl-user)

(unless (find-package :asdf)
  (require :asdf))

(asdf:oos 'asdf:load-op :llgs-engine-cl)

(in-package #:tubegame)

(defun disp-name-and-license ()
  (format t "~A~%~A~%~A~%" *GAMELABEL* *COPYRIGHT* *RIGHTS*))

(defun parse-cmdargs ()
  "TODO: parse command line parameters."
  (list "plugins_d.cfg" "tubegame.cfg" "tubegame.log" 'directx "resources_d.cfg" ))

(defun debugmodep (params)
  (if (equal "plugins_d.cfg" (first params))
      t
      nil))

(defun init-game (params)
  (let ((indebugmode (debugmodep params)))
  (format t "Initializing game in ~A mode.~%" 
	  (if indebugmode "DEBUG" "RELEASE"))
  (format t "Loading llgs engine...~A~%" (llgs-engine-cl:load-llgsengine indebugmode))
    (format t "Initialize rendering...~A~%" 
	    (llgs-engine-cl:render-init 
	     (first params) (second params) (third params)
	     (fourth params) (fifth params)))
    (format t "Creating renderwindow...~A~%" (llgs-engine-cl:render-createrenderwindow "tubegame"))
    (format t "Creating scenemanager...~A~%" (llgs-engine-cl:render-createscenemanager "INTERIOR" "tubescene"))
    (format t "Initializing camera and scene...~%")
    (setq *main-camera* (llgs-engine-cl:render-createcamera "main camera"))
    (setq *main-camera-node* (llgs-engine-cl:render-createscenenode *MAIN-CAMERA-SCENENODE-NAME*))
    (llgs-engine-cl:render-attachmoveable *main-camera-node* *main-camera*)
    (llgs-engine-cl:render-setscenenodepos *main-camera-node* (first *MAIN-CAMERA-INITIAL-POS*) 
					   (second *MAIN-CAMERA-INITIAL-POS*) (third *MAIN-CAMERA-INITIAL-POS*))
    (llgs-engine-cl:render-cameralookat *main-camera* (first *MAIN-CAMERA-INITIAL-LOOKAT*) 
					   (second *MAIN-CAMERA-INITIAL-LOOKAT*) (third *MAIN-CAMERA-INITIAL-LOOKAT*))
    (llgs-engine-cl:render-setcameranearclipdist *main-camera* 0.01)
    (llgs-engine-cl:render-setcamerafarclipdist *main-camera* 15000.0)
    (llgs-engine-cl:render-setcameraasviewport *main-camera*)
    (llgs-engine-cl:render-setviewportbackground 0.5 1.0 0.5)
    (llgs-engine-cl:render-setambientlight 0.15 0.15 0.15)
    (llgs-engine-cl:render-setskybox *SKYBOX-MAT*)
    (let ((light (llgs-engine-cl:render-createlight "mainlight")))
      (llgs-engine-cl:render-setlighttype light "POINT")
      (llgs-engine-cl:render-lightdiffcolor light 0.4 0.4 0.4)
      (llgs-engine-cl:render-setlightpos light 100.0 -100.0 0.0))
    (let ((light (llgs-engine-cl:render-createlight "mainlight2")))
      (llgs-engine-cl:render-setlighttype light "POINT")
      (llgs-engine-cl:render-lightdiffcolor light 0.4 0.4 0.4)
      (llgs-engine-cl:render-setlightpos light 100.0 100.0 0.0))
    )
  (format t "Gamelabel...~A~%" (llgs-engine-cl:render-createsimpletext "st_gamelabel" *GAMELABEL* "DroidSans-Bold" 16 0.0 0.0 500.0 14.0))
;  (llgs-engine-cl:render-simpletextcolor "st_gamelabel" 1.0 0.0 0.0)
;  (llgs-engine-cl:render-simpletextshow "st_gamelabel")
)

(defun game_run ()
  "Call this to start and run the game."
  (disp-name-and-license)
  (let ((params (parse-cmdargs))
	(maintimer nil) ; measuring time between frames
	(deltatime 0))
    (init-game params)
    (format t "Initializing input...~A~%" (llgs-engine-cl:input-init))
    (format t "Initializing colldet...~A~%" (llgs-engine-cl:colldet-init))
    (setq maintimer (llgs-engine-cl:timer-create))
    ; gameloop
    (loop until *game-should-exit* do
	 (llgs-engine-cl:input-capture)

	 ; simple gamestate :)
	 (if *in-game* 
	     (one-game-frame deltatime)
	     (one-startscreen-frame deltatime))

	 ; render one frame
	 (llgs-engine-cl:render-oneframe)
	 (setq deltatime (/ (llgs-engine-cl:timer-getmicroseconds maintimer) 1000000.0))

	 (llgs-engine-cl:timer-reset maintimer)

	 (if (llgs-engine-cl:input-keypressed *F11-KEY*) ; screenshot
	     (llgs-engine-cl:render-screenshottofile "tubegame-screenshot-"))
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*) ; end playing game
	     (setq *game-should-exit* t))
	 ))
  (format t "Shutdown input...~A~%" (llgs-engine-cl:input-shutdown))
  (format t "Shutdown colldet...~A~%" (llgs-engine-cl:colldet-shutdown))
  (format t "Shutdown renderer...~A~%" (llgs-engine-cl:render-shutdown))
  (llgs-engine-cl:close-llgsengine)
  (format t "Game end.~%"))
