An attempt to use twitter-bootstrap as html base for weblocks applications.

At this moment weblocks dependencies system should not work with this theme, but I'm using and recommend jquery-seq for dependencies and I have no problems with it.
I don't know will dependencies work in future or not.

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

And here are examples of how to add scripts to application 

# weblocks-jquery
oz@debian> git clone git://github.com/html/weblocks-jquery.git                                                                                                                                             ~/projects/temporary
Cloning into weblocks-jquery...
remote: Counting objects: 45, done.
remote: Compressing objects: 100% (31/31), done.
remote: Total 45 (delta 16), reused 35 (delta 6)
Receiving objects: 100% (45/45), 9.13 KiB, done.
Resolving deltas: 100% (16/16), done.
oz@debian> cd pub/scripts                                                                                                                                                                                  ~/projects/temporary
oz@debian> ln -s ../../weblocks-jquery/*.js .                                                                                                                                                  ~/projects/temporary/pub/scripts
oz@debian>   

# and jquery-seq
oz@debian> git clone git://github.com/html/jquery-seq.git                                                                                                                                                  ~/projects/temporary
Cloning into jquery-seq...
remote: Counting objects: 34, done.
remote: Compressing objects: 100% (23/23), done.
remote: Total 34 (delta 10), reused 33 (delta 9)
Receiving objects: 100% (34/34), 17.98 KiB, done.
Resolving deltas: 100% (10/10), done.
oz@debian> cd pub/scripts                                                                                                                                                                                  ~/projects/temporary
oz@debian> ln -s ../../jquery-seq/jquery-seq.js .                                                                                                                                              ~/projects/temporary/pub/scripts
oz@debian>                                                                                                                                                                                     ~/projects/temporary/pub/scripts


Also you can use following snippet for initial app for testing, it should be pretty displayed when all is ok. 

(defun init-user-session (root)
  (setf (widget-children root)
	(list (lambda (&rest args)
		(with-html
		  (:div :class "page-header" (:h1 "Happy Hacking!")))))))

