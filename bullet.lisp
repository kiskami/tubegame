;;;; tubegame is a simple game using llgs-engine-cl interface
;;;; Copyright (c) 2013 Kalman Kiss, Zalaegerszeg Hungary
;;;; All rights reserved.
;;;; kiskami@freemail.hu
;;;;
;;;; bullet.lisp

(in-package #:tubegame)

(defun create-bullet (type billbset x y z orinode)
  (let* ((startpos (calc-bullet-startpos x y z))
	 (physobj (llgs-engine-cl:colldet-addbox (first startpos) (second startpos) (third startpos)
						 *BULLETBOX-HALFEXT1* *BULLETBOX-HALFEXT2* *BULLETBOX-HALFEXT3*
						 *BULLET-PHYS-GRP* *BULLET-PHYS-MASK*))
	 (bulletent (make-bulletdata
		     :type type
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
    (llgs-engine-cl:colldet-setcolobjpos physobj (first startpos) (second startpos) (third startpos))
    (add-to-physobjmap physobj bulletent)
    bulletent))

(defun blow-bullet (bullet)
  (llgs-engine-cl:billboard-remove (bulletdata-billset bullet) (bulletdata-billboard bullet))
  (remove-entity bullet)
  (del-from-physobjmap (bulletdata-physobj bullet))
  (del-physobj (bulletdata-physobj bullet))
  (add-entity (create-bullet-explosion bullet)))

(defun bullet-update (bullet elapsedt)
  (let ((distdelta (* elapsedt *BULLETSPEED*)))
    (cond ((<= *BULLET-MAXDIST* (+ distdelta (bulletdata-flydist bullet)))
	   (blow-bullet bullet))
	  (t
	   (incf (bulletdata-flydist bullet) distdelta)
	   (bullet-flystep bullet distdelta)))))

(defun bullet-collfunc (bullet otherobj)
  (when otherobj
    (cond ((eq 'cube (entitydata-type otherobj))
	   (format t "collision with cube, destorying~%")
	   (blow-bullet bullet))
	  ((eq 'asteroid (entitydata-type otherobj))
	   (format t "collision with asteroid, damageing and destorying ~%")
	   (damage-asteroid otherobj (bulletdata-energy bullet))))))

(defun create-bullet-explosion (bullet)
  nil)

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
;    (format t "bullet-flystep: newpos: ~A~%" (llgs-engine-cl:billboard-getpos billset billb))
    (let ((pos (llgs-engine-cl:billboard-getpos billset billb)))
      (llgs-engine-cl:colldet-setcolobjpos (bulletdata-physobj bullet)
					   (first pos)
					   (second pos) 
					   (third pos)))
    ))
