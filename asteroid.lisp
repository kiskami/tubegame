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
		 :subtype type
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
  (declare (ignore ast elapsedt)))

(defun collision-asteroid (ast otherobj)
  (declare (ignore ast otherobj)))

(defun damage-asteroid (ast energy)
  (decf (asteroiddata-energy ast) energy)
  (cond ((<= (asteroiddata-energy ast) 0)
	 (setf (asteroiddata-energy ast) 0)
	 (blow-asteroid ast))
	(t 
;	 (format t "damaged asteroid with energy ~A - ~A~%" (asteroiddata-energy ast) energy )
	 nil)))

(defun blow-asteroid (ast)
;  (format t "asteroid blowup ~A~%" ast)
  (remove-entity ast)
  (del-physobj (asteroiddata-physobj ast))
  (del-from-physobjmap (asteroiddata-physobj ast))
  (add-entity (create-asteroid-explosion ast)) 
  (llgs-engine-cl:render-destroyscenenode (asteroiddata-node ast)))

(defun create-asteroid-explosion (ast)
  (create-explosion (llgs-engine-cl:render-getscenenodepos (asteroiddata-node ast))
		    (asteroiddata-subtype ast)))
