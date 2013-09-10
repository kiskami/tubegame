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
    (llgs-engine-cl:render-setscenenodepos *main-camera-node* 0.0 5.0 5.0)
    (llgs-engine-cl:render-cameralookat *main-camera* 0.0 0.0 0.0)
    (llgs-engine-cl:render-setcameranearclipdist *main-camera* 1.0)
    (llgs-engine-cl:render-setcamerafarclipdist *main-camera* 15000.0)
    (llgs-engine-cl:render-setcameraasviewport *main-camera*)
    (llgs-engine-cl:render-setviewportbackground 0.5 1.0 0.5)
    (llgs-engine-cl:render-setambientlight 0.15 0.15 0.15)
    (llgs-engine-cl:render-setskybox *SKYBOX-MAT*)
    (let ((light (llgs-engine-cl:render-createlight "mainlight")))
      (llgs-engine-cl:render-setlighttype light "POINT")
      (llgs-engine-cl:render-lightdiffcolor light 0.4 0.4 0.4)
      (llgs-engine-cl:render-setlightpos light 0.0 100.0 100.0))))

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
;	 (format t "0~%")
	 (llgs-engine-cl:input-capture)
;	 (format t "1~%")

	 ; simple gamestate :)
	 (if *in-game* 
	     (one-game-frame deltatime)
	     (one-startscreen-frame deltatime))
;	 (format t "2~%")

	 ; render one frame
	 (llgs-engine-cl:render-oneframe)
;	 (format t "3~%")
	 (setq deltatime (/ (llgs-engine-cl:timer-getmicroseconds maintimer) 1000000.0))
;	 (format t "4~%")

	 (llgs-engine-cl:timer-reset maintimer)
;	 (format t "5 - ~A~%" (llgs-engine-cl:input-keypressed *F11-KEY*))

	 (if (llgs-engine-cl:input-keypressed *F11-KEY*) ; screenshot
	     (llgs-engine-cl:render-screenshottofile "tubegame-screenshot-"))
	 (if (llgs-engine-cl:input-keypressed *ESC-KEY*) ; end playing game
	     (setq *game-should-exit* t))
;	 (format t "6~%")
	 ))
  (format t "Shutdown input...~A~%" (llgs-engine-cl:input-shutdown))
  (format t "Shutdown colldet...~A~%" (llgs-engine-cl:colldet-shutdown))
  (format t "Shutdown renderer...~A~%" (llgs-engine-cl:render-shutdown))
  (llgs-engine-cl:close-llgsengine)
  (format t "Game end.~%"))
