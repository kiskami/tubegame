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
