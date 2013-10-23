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
;;;; tubegame.asd

(asdf:defsystem #:tubegame
;  :serial t
  :description "tubegame is a simple game using llgs-engine-cl interface"
  :author "Kalman Kiss <kiskami@freemail.hu>"
  :version "0.0.1"
  :license "GPL2"
  :depends-on (#:llgs-engine-cl)
  :components ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "utils" :depends-on ("globals"))
	       (:file "startdest" :depends-on ("package" "globals"))
	       (:file "explosion" :depends-on ("package" "globals" "utils"))
	       (:file "asteroid" :depends-on ("package" "globals" "utils" 
							"explosion"))
	       (:file "bullet" :depends-on ("package" "globals" "utils"))
	       (:file "player" :depends-on ("package" "globals" "utils" 
						      "bullet" "explosion"))
	       (:file "game" :depends-on ("package" "globals" "utils" 
						    "player" 
						    "startdest" 
						    "asteroid" 
						    "explosion"))
	       (:file "startscreen" :depends-on ("package"))
               (:file "tubegame" :depends-on ("package" "globals" "game" "startscreen"))))
