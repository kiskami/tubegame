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

(defparameter *main-camera* nil "Main game camera.")
(defparameter *main-camera-node* nil "Main game camera scenenode")
(defconstant *MAIN-CAMERA-SCENENODE-NAME* "main cam scenenode")

(defconstant *SKYBOX-MAT* "backgrounds/FirstSimpleStarField" "Skybox material name.")
(defconstant *LEVEL1-FILE* "../data/level1.lisp" "Game level 1 file.")

(defconstant *ESC-KEY* 1 "OIS scan code for ECS key.")
(defconstant *F11-KEY* #x57 "OIS scan code for F11 key.")
(defconstant *F12-KEY* #x58 "OIS scan code for F12 key.")
(defconstant *W-key* #x11)
(defconstant *S-key* #x1F)
(defconstant *A-key* #x1E)
(defconstant *D-key* #x20)

; ------------------------------------------------

(defconstant *ONEDEGREE* (/ pi 180.0) "One degree in radians.")

; ------------------------------------------------

(defconstant *PLAYER-MESH* "Playership.mesh" "Player ship Ogre mesh resource name")
(defconstant *PLAYER-MESH-NAME* "playermesh")
(defconstant *PLAYER-NODE-NAME* "playernode")

(defconstant *CUBE1-MESH* "Cube1.mesh" "Cube 1 Ogre mesh resource name")
(defconstant *CUBE2-MESH* "Cube2.mesh" "Cube 2 Ogre mesh resource name")
(defconstant *CUBE-2-MESH* "Cube-2.mesh" "Cube -2 Ogre mesh resource name")
(defconstant *CUBE3-MESH* "Cube3.mesh" "Cube 3 Ogre mesh resource name")
(defconstant *CUBE-3-MESH* "Cube-3.mesh" "Cube -3 Ogre mesh resource name")
(defconstant *CUBE4-MESH* "Cube4.mesh" "Cube 4 Ogre mesh resource name")
(defconstant *CUBE-4-MESH* "Cube-4.mesh" "Cube 4 Ogre mesh resource name")
(defconstant *CUBE5-MESH* "Cube5.mesh" "Cube 5 Ogre mesh resource name")
(defconstant *CUBE6-MESH* "Cube6.mesh" "Cube 6 Ogre mesh resource name")

(defconstant *ASTEROID1-MESH* "Asteroida1.mesh" "Asteroid 1 Ogre mesh resource name")
(defconstant *ASTEROID2-MESH* "Asteroida2.mesh" "Asteroid 2 Ogre mesh resource name")
(defconstant *ASTEROID3-MESH* "Asteroida3.mesh" "Asteroid 3 Ogre mesh resource name")

; ------------------------------------------------

(defparameter *PHYSOBJMAP* (make-hash-table) "Physics objects pointer->entity map.")

(defstruct leveldata
  levelfile
  levelpointmargin
  playershieldenergystart	
  playerstructintegritystart	
  playerweaponenergystart
  entities)

(defstruct entitydata
  type
  mesh
  node
  physobj
  updatefunc
  collfunc)

(defstruct (playerdata (:include entitydata))
  levelpoints
  integrity
  weaponenergy
  shieldenergy
  activepowerups)

(defstruct (asteroiddata (:include entitydata))
  energy)
