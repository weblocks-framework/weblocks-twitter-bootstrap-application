;;;; weblocks-twitter-bootstrap-application.asd

(asdf:defsystem #:weblocks-twitter-bootstrap-application
  :serial t
  :description "Describe weblocks-twitter-bootstrap-application here"
  :author "Olexiy Zamkoviy <olexiy.z@gmail.com>"
  :license "LLGPL"
  :depends-on (:cl-mustache :weblocks :yaclml)
  :components ((:file "package")
               (:file "weblocks-twitter-bootstrap-application")))

