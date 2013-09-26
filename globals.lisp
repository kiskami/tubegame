;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; globals.lisp

(in-package #:tubegame)

(defconstant *GAMELABEL* "tubegame - a 3D space shoot'em up")
(defconstant *COPYRIGHT* "Copyright (C) 2013 Kalman Kiss <kiskami@freemail.hu>, Hungary")
(defconstant *RIGHTS* "All rights reserved.")

; ------------------------------------------------

(defconstant *LABELCOLOR* '(0.24 0.61 0.83) "Skyblue color r,g,b")

(defparameter *game-should-exit* nil "Game loop ends when t.")
(defparameter *in-game* nil "Player is playing on a map when t, startscreen is displayed othervise.")

(defparameter *main-camera* nil "Main game camera.")
(defparameter *main-camera-node* nil "Main game camera scenenode")

(defconstant *MAIN-CAMERA-SCENENODE-NAME* "main cam scenenode")
(defconstant *MAIN-CAMERA-INITIAL-POS* '(-1.0 1.0 0.0))
(defconstant *MAIN-CAMERA-INITIAL-LOOKAT* '(0.0 0.0 0.0))

(defconstant *PLAYER-INITIAL-POS* '(0.0 0.0 0.0))
(defconstant *PLAYER-NODE-SCALE* '(0.01 0.01 0.01))

(defconstant *ONEDEGREE* (/ pi 180.0) "One degree in radians.")

(defconstant *TURNSPEED* (* *ONEDEGREE* 14))
(defconstant *ROLLSPEED* (* *ONEDEGREE* 12))
(defconstant *FLYSPEED* 1)
(defconstant *BULLETSPEED* 1.75)

(defconstant *SKYBOX-MAT* "backgrounds/FirstSimpleStarField" "Skybox material name.")
(defconstant *LEVEL1-FILE* "../data/level1.lisp" "Game level 1 file.")

(defconstant *ESC-KEY* 1 "OIS scan code for ESC key.")
(defconstant *F10-KEY* #x56 "OIS scan code for F10 key.")
(defconstant *F11-KEY* #x57 "OIS scan code for F11 key.")
(defconstant *F12-KEY* #x58 "OIS scan code for F12 key.")
(defconstant *W-key* #x11)
(defconstant *S-key* #x1F)
(defconstant *A-key* #x1E)
(defconstant *D-key* #x20)

(defconstant *FPSDISPTIME* 2)
(defconstant *COLLDET-DEBUGDRAWER-TIMEOUT* 1.5)
(defconstant *COLLDET-TIMEOUT* 0.05 "Collision detection frequency.")

(defconstant *FLYMODE-SWITCH-TIMEOUT* 1.5)

; ------------------------------------------------

(defconstant *PLAYER-MESH* "Playership.mesh" "Player ship Ogre mesh resource name")
(defconstant *PLAYER-MESH-NAME* "playermesh")
(defconstant *PLAYER-NODE-NAME* "playernode")
(defconstant *PLAYER-PITCHNODE-NAME* "player_pitchnode")

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

(defconstant *ASTEROID1-SCALE* 0.2)
(defconstant *ASTEROID2-SCALE* 0.12)
(defconstant *ASTEROID3-SCALE* 0.05)

(defconstant *EXPLOSION-AST1-W* 1.0)
(defconstant *EXPLOSION-AST1-H* 1.0)
(defconstant *EXPLOSION-AST2-W* 0.5)
(defconstant *EXPLOSION-AST2-H* 0.5)
(defconstant *EXPLOSION-AST3-W* 0.2)
(defconstant *EXPLOSION-AST3-H* 0.2)

; ------------------------------------------------

(defconstant *PLAYER-PHYS-GRP* 1)
(defconstant *PLAYER-PHYS-MASK* (+ 2 4 8 16 32 64 128))
(defconstant *BULLET-PHYS-GRP* 2)
(defconstant *BULLET-PHYS-MASK* (+ 4 8 32 128))
(defconstant *CUBE-PHYS-GRP* 4)
(defconstant *CUBE-PHYS-MASK* (+ 1 2 64 128))
(defconstant *ASTEROIDA-PHYS-GRP* 8)
(defconstant *ASTEROIDA-PHYS-MASK* (+ 1 2 64 128))

; ------------------------------------------------

(defconstant *ASTEROID-BOUNCE-PENALTY* 5)
(defconstant *CUBE-BOUNCE-PENALTY* 3)

(defconstant *PLAYER-FIRE-TIMEOUT* 0.5)

(defconstant *ASTEROID1-ENERGY* 100)
(defconstant *ASTEROID2-ENERGY* 60)
(defconstant *ASTEROID3-ENERGY* 20)

(defconstant *BULLET-ENERGY* 20)
(defconstant *BULLET-MAXDIST* 5)

(defconstant *PLAYER-BULLET-W* 0.2)
(defconstant *PLAYER-BULLET-H* 0.2)
(defconstant *PLAYER-BULLET-MAT* "Examples/Flare")

(defconstant *BULLETBOX-HALFEXT1* 0.015 "Player bullet colldet box size")
(defconstant *BULLETBOX-HALFEXT2* 0.015)
(defconstant *BULLETBOX-HALFEXT3* 0.015)

(defconstant *EXPLOSION-MAT* "Explosion33")
(defconstant *EXPBILLSET-STACKS* 8)
(defconstant *EXPBILLSET-SLICES* 8)
(defconstant *EXPLOSION-LIFETIME* 2)

(defconstant *AST1-EXPLOSION-DIST* 0.0)
(defconstant *AST2-EXPLOSION-DIST* 0.0)
(defconstant *AST3-EXPLOSION-DIST* 0.0)

; ------------------------------------------------

(defconstant *POINTSPANELID* "player_points")
(defconstant *INTEGPANELID*  "player_integrity")
(defconstant *WEAPONPANELID* "player_weapon_energy")
(defconstant *SHIELDPANELID* "player_shield_energy")

; ------------------------------------------------

(defparameter *PHYSOBJMAP* (make-hash-table) "Physics objects pointer->entity map.")
(defparameter *PHYSOBJMAP-TRASH* nil)
(defparameter *ENTITIES* '() "Game entities list.")

(defparameter *EXPBILLBSET* nil "Explosions BillboardSet pointer")
(defparameter *EXPBILLBSETNODE* nil)

(defstruct leveldata
  levelfile
  levelpointmargin
  playershieldenergystart	
  playerstructintegritystart
  playerweaponenergystart
  startposlist)

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
  bouncetimer
  bouncing
  firetime
  firing
  movementdir
  relx
  rely
  bulletbillbnode
  bulletbillbset
  flymode
  pitchnode)

(defstruct (asteroiddata (:include entitydata))
  subtype
  rotx
  roty
  rotz
  energy)

(defstruct (bulletdata (:include entitydata))
  owner
  billboard
  billset
  energy
  lifetime
  flydist
  pos
  flydir)

(defstruct (explosiondata (:include entitydata))
  billboard
  lifetime
  texindex
  pos)
