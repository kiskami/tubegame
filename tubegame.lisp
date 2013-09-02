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

(defun game_run ()
  "Call this to start and run game."
  (format t "~A~%~A~%~A~%" "tubegame is a simple game using llgs-engine-cl interface"
	  "Copyright (C) 2013 Kalman Kiss <kiskami@freemail.hu>, Hungary"
	  "All rights reserved.")
  (format t "Loading llgs engine in DEBUG mode...~A~%"  (llgs-engine-cl:load-llgsengine t))
  (format t "Initialize rendering...~A~%" (llgs-engine-cl:render-init "plugins_d.cfg" "tubegame.cfg" "tubegame.log" 'directx "resources_d.cfg"))
  (format t "Creating renderwindow...~A~%" (llgs-engine-cl:render-createrenderwindow "tubegame"))
  (format t "Creating scenemanager...~A~%" (llgs-engine-cl:render-createscenemanager "INTERIOR" "tubescene")))
