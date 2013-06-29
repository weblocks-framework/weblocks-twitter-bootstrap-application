;;;; weblocks-twitter-bootstrap-application.asd

(asdf:defsystem #:weblocks-twitter-bootstrap-application
  :serial t
  :description "Bootstrap skin for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :version "0.0.22"
  :depends-on (:cl-mustache :weblocks :yaclml :cl-fad)
  :components ((:file "package")
               (:file "weblocks-twitter-bootstrap-application")
               (:file "make-new-bootstrap-app" :depends-on ("package"))))

