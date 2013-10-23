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
;;;; explosion.lisp

(in-package #:tubegame)

(defun init-explosions (parentnode)
  "Initialize global BillboardSet for explosions."
  (setf *EXPBILLBSET* (llgs-engine-cl:billboardset-create))
  (setf *EXPBILLBSETNODE* (llgs-engine-cl:render-createchildscenenode
;			   (llgs-engine-cl:render-rootscenenode)
			   parentnode
			   "explosion_billboard" :inheritori 1 :inheritscale 0))
  (llgs-engine-cl:billboardset-setdefdims *EXPBILLBSET* 
					  *EXPLOSION-AST1-W* *EXPLOSION-AST1-H*)
  (llgs-engine-cl:billboardset-setmaterial *EXPBILLBSET* *EXPLOSION-MAT*)
  (llgs-engine-cl:billboardset-stacksandslices *EXPBILLBSET* 
					       *EXPBILLSET-STACKS*
					       *EXPBILLSET-SLICES*)
  (llgs-engine-cl:render-attachmoveable *EXPBILLBSETNODE* *EXPBILLBSET*)
;  (format t "Initialized node and billboardset for explosions. ~A at ~A, ~A~%"
;	  *EXPBILLBSETNODE*
;	  (llgs-engine-cl:render-getscenenodepos *EXPBILLBSETNODE*)
;	  *EXPBILLBSET*)
  )

(defun reset-explosions ()
  (llgs-engine-cl:billboardset-clear *EXPBILLBSET*))

(defun create-explosion (entpos enttype)
  (let* ((pos (calc-explosion-pos 
	       (llgs-engine-cl:render-getscenenodepos *main-camera-node*)
	       entpos 
	       enttype))
	 (dims (calc-explosion-dims enttype))
	 (expent (make-explosiondata
		  :type 'explosion
		  :updatefunc #'update-explosion
		  :billboard (llgs-engine-cl:billboard-create *EXPBILLBSET*
							      (first pos)
							      (second pos)
							      (third pos)
							      1.0 1.0 1.0)
		  :lifetime 0
		  :texindex 0
		  :pos pos)))
    (llgs-engine-cl:billboard-settexcoordind *EXPBILLBSET*
					     (explosiondata-billboard expent)
					     (explosiondata-texindex expent))
;    (format t "create-explosion: campos ~A, entpos ~A, exppos ~A~%" 
;	    (llgs-engine-cl:render-getscenenodepos *main-camera-node*)
;	    entpos 
;	    pos)
    (if dims
	(llgs-engine-cl:billboard-setdims *EXPBILLBSET*
					  (explosiondata-billboard expent)
					  (first dims) (second dims)))
    (llgs-engine-cl:billboard-updatebounds *EXPBILLBSET*)
    expent))

;(defun calc-explosion-pos_ (campos entpos enttype)
;  (list (first entpos) (second entpos) (third entpos)))

(defun calc-explosion-pos (campos entpos enttype)
  (let ((dirvec (vec3_normalize (vec3- entpos campos))))
    (cond ((eq 'ast1 enttype)
	   (vec3- entpos (vec3* dirvec *AST1-EXPLOSION-DIST*)))
	  ((eq 'ast2 enttype)
	   (vec3- entpos (vec3* dirvec *AST2-EXPLOSION-DIST*)))
	  ((eq 'ast3 enttype)
	   (vec3- entpos (vec3* dirvec *AST3-EXPLOSION-DIST*)))
	  (t
	   (format t "calc-explosion-pos: unknown entity type: ~A~%" enttype)
	   entpos))))

(defun calc-explosion-dims (enttype)
  (cond ((eq 'ast1 enttype) nil)
	((eq 'ast2 enttype) (list *EXPLOSION-AST2-W* *EXPLOSION-AST2-H*))
	((eq 'ast3 enttype) (list *EXPLOSION-AST3-W* *EXPLOSION-AST3-H*))
	(t
	 (format t "calc-explosion-dims: unknown entity type: ~A~%" enttype))))

(defun update-explosion (exp elapsedt)
  (incf (explosiondata-lifetime exp) elapsedt)
  (when (explosion-needs-next-tex exp)
    (when (< (explosiondata-texindex exp) (1-(* *EXPBILLSET-SLICES* *EXPBILLSET-STACKS*)))
	   (incf (explosiondata-texindex exp))
	   (llgs-engine-cl:billboard-settexcoordind *EXPBILLBSET*
						    (explosiondata-billboard exp)
						    (explosiondata-texindex exp))))
  (when (<= *EXPLOSION-LIFETIME* (explosiondata-lifetime exp))
;    (format t "update-explosion: end of lifetime~%")
    (llgs-engine-cl:billboard-remove *EXPBILLBSET* (explosiondata-billboard exp))
    (remove-entity exp)))

(defun explosion-needs-next-tex (exp)
  (let ((timeperslice (/ *EXPLOSION-LIFETIME* (* *EXPBILLSET-SLICES* *EXPBILLSET-STACKS*))))
    (<=
     (* (explosiondata-texindex exp) timeperslice)
     (explosiondata-lifetime exp))))
