An attempt to use twitter-bootstrap as html base for weblocks applications.

To use this theme load this package and use following snippet when defining application. You need also to add jquery-seq and weblocks-jquery scripts to pub/scripts directory.

; Example of defwebapp
(defwebapp media-library
           :prefix "/" 
           :description "Media library"
           ; Bottom is important string
           :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
           :init-user-session 'media-library::init-user-session
           :autostart nil                   ;; have to start the app manually
           :debug t)

Also you can use following snippet for initial app for testing, it should be pretty displayed when all is ok. 

(defun init-user-session (root)
  (setf (widget-children root)
	(list (lambda (&rest args)
		(with-html
		  (:div :class "page-header" (:h1 "Happy Hacking!")))))))

Package uses weblocks assets so all dependencies should be installed automatically.
