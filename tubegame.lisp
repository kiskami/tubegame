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
;;;; tubegame.lisp

(in-package :cl-user)

(unless (find-package :asdf)
  (require :asdf))

(asdf:oos 'asdf:load-op :llgs-engine-cl)

(in-package #:tubegame)

(defun disp-name-and-license ()
  (format t "~A~%~A~%~A~%" *GAMELABEL* *COPYRIGHT* *RIGHTS*))

(defun conv-to-int (str)
  (parse-integer str :junk-allowed t))    

(defun parse-cmdargs (args)
  "Parse command line and return game initialization parameters."
  (format t "parse-cmdargs: ~A~%" args)
  (let ((pluginsfile "plugins.cfg")
	(cfgfile "tubegame.cfg")
	(logfile "tubegame.log")
	(rendersys 'opengl) ;'directx)
	(resfile "resources.cfg")
	(w 800) (h 600) (fullscreen 0)
	(help nil) (debug nil)
	(wp (member "-w" args :test #'equal))
	(hp (member "-h" args :test #'equal))
	(fp (member "-f" args :test #'equal))
	(?p (member "-?" args :test #'equal)))
;    (break)
    (if (and wp (conv-to-int (second wp))) (setf w (conv-to-int (second wp))))
    (if (and hp (conv-to-int (second hp))) (setf h (conv-to-int (second hp))))
    (if fp (setf fullscreen 1))
    (if ?p (setf help t))
    (list pluginsfile cfgfile logfile rendersys resfile w h fullscreen help debug)))

(defun debugmodep (params)
  (first (last params)))
 
(defun init-game (params)
  (let ((indebugmode (debugmodep params)))
    (format t "Initializing game in ~A mode.~%Params: ~A\%" 
	    (if indebugmode "DEBUG" "RELEASE") params)
    (format t "Loading llgs engine...~A~%" (llgs-engine-cl:load-llgsengine indebugmode))
    (format t "Initialize rendering...~A~%" 
	    (llgs-engine-cl:render-init 
	     (first params) (second params) (third params)
	     (fourth params) (fifth params)))
    (format t "Creating renderwindow (~Ax~A.~A)...~A~%" (sixth params) (seventh params) (eighth params)
	    (llgs-engine-cl:render-createrenderwindow "tubegame" 
						      :w (sixth params) 
						      :h (seventh params)
						      :fullscreen (eighth params)))
    (format t "Creating scenemanager...~A~%" (llgs-engine-cl:render-createscenemanager 
					      "INTERIOR" 
					      "tubescene"))
    (format t "Initializing camera and scene...~%")
    (setq *main-camera* (llgs-engine-cl:render-createcamera "main camera"))
    (setq *main-camera-node* (llgs-engine-cl:render-createscenenode *MAIN-CAMERA-SCENENODE-NAME*))
    (llgs-engine-cl:render-attachmoveable *main-camera-node* *main-camera*)
    (llgs-engine-cl:render-setscenenodepos *main-camera-node* (first *MAIN-CAMERA-INITIAL-POS*) 
					   (second *MAIN-CAMERA-INITIAL-POS*) (third *MAIN-CAMERA-INITIAL-POS*))
    (llgs-engine-cl:render-cameralookat *main-camera* (first *MAIN-CAMERA-INITIAL-LOOKAT*) 
					   (second *MAIN-CAMERA-INITIAL-LOOKAT*) (third *MAIN-CAMERA-INITIAL-LOOKAT*))
    (llgs-engine-cl:render-setcameranearclipdist *main-camera* 0.01)
    (llgs-engine-cl:render-setcamerafarclipdist *main-camera* 10000.0)
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
    ))

(defun game-run ()
  "Call this to start and run the game."
  (disp-name-and-license)
  (let ((params (parse-cmdargs CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*)) ; not working!?
	(maintimer nil) ; measuring time between frames
	(deltatime 0)
	(fpsdisptime 0))
    (when (car (last params 2))
      (format t "Command line params: -- [-w <res_width>] [-h <res_height> [-f] [-d] [-?]
  --     - mandatory separator
  -w -h  - renderwindow resolution - width, height in pixels
  -f     - run on fullscreen
  -d     - debug mode, always on atm
  -?     - this help~%")
      (return-from game-run))
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

	 (incf fpsdisptime deltatime)
	 (when (>= fpsdisptime *FPSDISPTIME*)
	   (format t "FPS act: ~A, min: ~A, max: ~A~%Tris: ~A, batches: ~A~%"
		   (llgs-engine-cl:render-actfps) (llgs-engine-cl:render-minfps) (llgs-engine-cl:render-maxfps) 
		   (llgs-engine-cl:render-trianglecount) (llgs-engine-cl:render-batchcount))
	   (setf fpsdisptime 0))))
  (format t "Shutdown input...~A~%" (llgs-engine-cl:input-shutdown))
  (format t "Shutdown colldet...~A~%" (llgs-engine-cl:colldet-shutdown))
  (format t "Shutdown renderer...~A~%" (llgs-engine-cl:render-shutdown))
  (llgs-engine-cl:close-llgsengine)
  (format t "Game end.~%"))
