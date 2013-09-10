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
	       (:file "startdest" :depends-on ("package" "globals"))
	       (:file "asteroid" :depends-on ("package" "globals"))
	       (:file "player" :depends-on ("package" "globals"))
	       (:file "game" :depends-on ("package" "globals" "player" "startdest" "asteroid"))
	       (:file "startscreen" :depends-on ("package"))
               (:file "tubegame" :depends-on ("package" "globals" "game" "startscreen"))))
