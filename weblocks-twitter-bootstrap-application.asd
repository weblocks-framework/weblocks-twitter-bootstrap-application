;;;; weblocks-twitter-bootstrap-application.asd

(asdf:defsystem #:weblocks-twitter-bootstrap-application
  :serial t
  :description "Bootstrap skin for weblocks"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :version "0.0.8"
  :depends-on (:cl-mustache :weblocks :yaclml)
  :components ((:file "package")
               (:file "weblocks-twitter-bootstrap-application")))

