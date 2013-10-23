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
;;;; bullet.lisp

(in-package #:tubegame)

(defun create-bullet (type billbset x y z orinode owner)
  (let* ((startpos (calc-bullet-startpos x y z))
	 (physobj (llgs-engine-cl:colldet-addbox (first startpos) (second startpos) (third startpos)
						 *BULLETBOX-HALFEXT1* *BULLETBOX-HALFEXT2* *BULLETBOX-HALFEXT3*
						 *BULLET-PHYS-GRP* *BULLET-PHYS-MASK*))
	 (bulletent (make-bulletdata
		     :type type
		     :owner owner
		     :physobj physobj
		     :updatefunc #'bullet-update
		     :collfunc #'bullet-collfunc
		     :energy *BULLET-ENERGY*
		     :pos startpos
		     :flydir (llgs-engine-cl:render-getscenenodeorientation orinode)
		     :flydist 0
		     :billset billbset
		     :billboard (llgs-engine-cl:billboard-create billbset 
								 (first startpos) (second startpos) (third startpos)
								 1.0 1.0 1.0))))
;    (format t "create-bullet: startpos: ~A, phsyobj: ~A~%" startpos physobj)
    (llgs-engine-cl:billboard-updatebounds billbset)
    (llgs-engine-cl:colldet-setcolobjpos physobj (first startpos) (second startpos) (third startpos))
    (add-to-physobjmap physobj bulletent)
    bulletent))

(defun blow-bullet (bullet)
  (llgs-engine-cl:billboard-remove (bulletdata-billset bullet) (bulletdata-billboard bullet))
  (remove-entity bullet)
  (del-from-physobjmap (bulletdata-physobj bullet))
  (del-physobj (bulletdata-physobj bullet)))

(defun bullet-update (bullet elapsedt)
  (let ((distdelta (* elapsedt *BULLETSPEED*)))
	   (incf (bulletdata-flydist bullet) distdelta)
	   (cond ((<= *BULLET-MAXDIST* (bulletdata-flydist bullet))
		  (blow-bullet bullet))
		 (t
		  (bullet-flystep bullet distdelta)))))

(defun bullet-collfunc (bullet otherobj)
  (when otherobj
    (cond ((eq 'cube (entitydata-type otherobj))
;	   (format t "collision with cube, destroying~%")
	   )
	  ((eq 'asteroid (entitydata-type otherobj))
;	   (format t "collision with asteroid, damageing and destroying ~%")
	   (add-points-to-owner (bulletdata-owner bullet) (bulletdata-energy bullet))
	   (damage-asteroid otherobj (bulletdata-energy bullet))))
    (blow-bullet bullet)))

(defun calc-bullet-startpos (x y z)
  (list x y z))

(defun bullet-flystep (bullet delta)
  (let ((dir (bulletdata-flydir bullet))
	(billb (bulletdata-billboard bullet))
	(billset (bulletdata-billset bullet)))
;    (format t "bullet-flystep: delta: ~A oripos: ~A flydir: ~A~%" delta (llgs-engine-cl:billboard-getpos billset billb) dir)
    (llgs-engine-cl:billboard-move billset billb
				   (first dir) (second dir)
				   (third dir) (fourth dir)
				   delta)
    (llgs-engine-cl:billboard-updatebounds billset)
;    (format t "bullet-flystep: newpos: ~A~%" (llgs-engine-cl:billboard-getpos billset billb))
    (let ((pos (llgs-engine-cl:billboard-getpos billset billb)))
      (llgs-engine-cl:colldet-setcolobjpos (bulletdata-physobj bullet)
					   (first pos)
					   (second pos) 
					   (third pos)))
    ))

(defun add-points-to-owner (player points)
  (incf (playerdata-levelpoints player) points))
