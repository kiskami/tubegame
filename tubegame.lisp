;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; tubegame.lisp

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
  (format t "Loading llgs engine...~A~%"  (llgs-engine-cl:load-llgsengine indebugmode))
  (format t "Initialize rendering...~A~%" 
	  (llgs-engine-cl:render-init 
	   (first params) (second params) (third params)
	   (fourth params) (fifth params)))
  (format t "Creating renderwindow...~A~%" (llgs-engine-cl:render-createrenderwindow "tubegame"))
  (format t "Creating scenemanager...~A~%" (llgs-engine-cl:render-createscenemanager "INTERIOR" "tubescene"))))

(defun update-input ()
  "Receive input events."
  nil)

(defun one-game-frame (elapsedt)
  "Update state and render one game playing frame."
  (if (< 0 elapsedt)
      nil
      nil))

(defun one-startscreen-frame (elapsedt)
  "Render one scrartscreen frame."
  (if (< 0 elapsedt)
      nil
      nil))

(defun game_run ()
  "Call this to start and run the game."
  (disp-name-and-license)
  (let ((params (parse-cmdargs)))
    (init-game params)
    (loop while (not *game-should-exit*) do
;	 (format t "Rendering...~%")
	 (update-input)
	 ; simple gamestate :)
	 (if *in-game* 
	     (one-game-frame 0)
	     (one-startscreen-frame 0))
	 (setq *game-should-exit* t)))
  (format t "Game end.~%"))
