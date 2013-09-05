;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; globals.lisp

(in-package #:tubegame)

(defconstant *GAMELABEL* "tubegame is a simple game using llgs-engine-cl interface")
(defconstant *COPYRIGHT* "Copyright (C) 2013 Kalman Kiss <kiskami@freemail.hu>, Hungary")
(defconstant *RIGHTS* "All rights reserved.")

; ------------------------------------------------

(defparameter *game-should-exit* nil "Game loop ends when t.")
(defparameter *in-game* nil "Player is playing on a map when t, startscreen is displayed othervise.")
;(defparameter *main-timer* nil "Main game timer.")
(defparameter *main-camera* nil "Main game camera.")

(defconstant *SKYBOX-MAT* "backgrounds/FirstSimpleStarField" "Skybox material name.")
(defconstant *LEVEL1-FILE* "../tubegame/level1.lisp" "Level 1 file.")

(defconstant *ESC-KEY* 1 "OIS scan code for ECS key.")
(defconstant *F11-KEY* #x57 "OIS scan code for F11 key.")
