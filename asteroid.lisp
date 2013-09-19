;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; asdteroid.lisp

(in-package #:tubegame)

(defun create-asteroid (x y z type n)
  (dotimes (i n)
    (create-1asteroid x y z type)))

(defun create-1asteroid (x y z type)
  (let* ((mesh (llgs-engine-cl:mesh-load (gen-name "ast_mesh" type x y z) (get-ast-meshfile-name type)))
	(node (llgs-engine-cl:render-createscenenode (gen-name "ast" type x y z)))
	(pos (list (+ (* 2 x) (get-rnd-coord 0.62)) 
		   (+ (* 2 y) (get-rnd-coord 0.62)) 
		   (+ (* 2 z) (get-rnd-coord 0.62))))
	(astent (make-asteroiddata
		 :type 'asteroid
		 :mesh mesh
		 :node node
		 :physobj (llgs-engine-cl:colldet-addmeshgeom (first pos) (second pos) (third pos)
							      mesh *ASTEROIDA-PHYS-GRP* *ASTEROIDA-PHYS-MASK*)
		 :updatefunc #'update-asteroid
		 :collfunc #'collision-asteroid
		 :rotx 0
		 :roty 0
		 :rotz 0
		 :energy (get-asteroid-energy type)))
	(scale (get-asteroid-scale type)))
    (llgs-engine-cl:render-attachmoveable node mesh)
    (llgs-engine-cl:render-addchild (llgs-engine-cl:render-rootscenenode) node)
    (llgs-engine-cl:render-setscenenodepos node (first pos) (second pos) (third pos))
    (llgs-engine-cl:render-setscenenodescale node (first scale) (second scale) (third scale))
    (llgs-engine-cl:colldet-syncolobjtoscenenode (asteroiddata-physobj astent) node)
    (llgs-engine-cl:colldet-setscale (asteroiddata-physobj astent) (first scale) (second scale) (third scale))
    (add-to-physobjmap (asteroiddata-physobj astent) astent)))

(defun get-ast-meshfile-name (type)
  (cond ((eq type 'ast2) *ASTEROID2-MESH*)
	((eq type 'ast3) *ASTEROID3-MESH*)
	(t *ASTEROID1-MESH*)))

(defun get-asteroid-energy (type)
  (cond ((eq type 'ast2) *ASTEROID2-ENERGY*)
	((eq type 'ast3) *ASTEROID3-ENERGY*)
	(t *ASTEROID1-ENERGY*)))

(defun get-asteroid-scale (type)
  (cond ((eq type 'ast2) (list *ASTEROID2-SCALE* *ASTEROID2-SCALE* *ASTEROID2-SCALE*))
	((eq type 'ast3) (list *ASTEROID3-SCALE* *ASTEROID3-SCALE* *ASTEROID3-SCALE*))
	(t (list *ASTEROID1-SCALE* *ASTEROID1-SCALE* *ASTEROID1-SCALE*))))

(defun update-asteroid (ast elapsedt)
; ! collobj sync
  nil)

(defun collision-asteroid (ast otherobj)
; ! collobj sync
  nil)
