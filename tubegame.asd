;;;; tubegame.asd

(asdf:defsystem #:tubegame
  :serial t
  :description "tubegame is a simple game using llgs-engine-cl interface"
  :author "Kalman Kiss <kiskami@freemail.hu>"
  :version "0.0.1"
  :license "GPL2"
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "globals")
               (:file "tubegame")))
