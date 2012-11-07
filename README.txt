An attempt to use twitter-bootstrap as html base for weblocks applications.

To use this theme load this package and use following snippet when defining application.

(defwebapp media-library
           :prefix "/" 
           :description "Media library"
           ; Bottom is important string
           :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
           :init-user-session 'media-library::init-user-session
           :autostart nil                   ;; have to start the app manually
           :debug t)

