# Weblocks Twitter Bootstrap Application Package

## About

An attempt to use Twitter Bootstrap as html base for Weblocks applications.

## Usage 

To use this theme load this package and use following snippet when defining application.

```lisp
; Example of defwebapp
(defwebapp media-library
           :prefix "/" 
           :description "Media library"
           ; Bottom is important string
           :subclasses (weblocks-twitter-bootstrap-application:twitter-bootstrap-webapp)
           :init-user-session 'media-library::init-user-session
           :autostart nil                   ;; have to start the app manually
           :debug t)
```

Also you can use following snippet in your initial app for testing, it should be pretty displayed when all is ok. 

```lisp
(defun init-user-session (root)
  (setf (widget-children root)
	(list (lambda (&rest args)
		(with-html
		  (:div :class "page-header" (:h1 "Happy Hacking!")))))))
```

## Additional Navigation Template

Here is snippet for making your navigation look like this http://getbootstrap.com/2.3.2/components.html#navbar

```lisp
(make-navigation 
   "navigation-name"
   ; ... some code with navigation items
   :navigation-class 'bootstrap-navbar-navigation)
```

## Assets dependencies

Package uses weblocks assets so all dependencies should be installed automatically.