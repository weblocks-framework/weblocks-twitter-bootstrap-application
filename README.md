# Weblocks Twitter Bootstrap Application

## About

Twitter Bootstrap skin for Weblocks. 
Supports forms (most of field types), gridedit (with pagination), navigation.

## Usage 

Inherit your application from `'twitter-bootstrap-webapp` to make skin work.

Use following snippet when defining application for this.

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

There is an additional navigation template in theme package.
Here is snippet for making your navigation look like this http://getbootstrap.com/2.3.2/components.html#navbar

```lisp
(make-navigation 
   "navigation-name"
   ; ... some code with navigation items
   :navigation-class 'bootstrap-navbar-navigation)
```

## Additional Navigation Widget

Package contains replacement for standard Weblocks navigation widget. It uses bootstrap navbar look - http://getbootstrap.com/2.3.2/components.html#navbar
It does not work with urls properly beacause of different routing system not yet included into Weblocks. But it may be used as tab widget.
Replace `make-navigation` with `weblocks-twitter-bootstrap-application:make-navbar-selector` and remove any keyword parameters from call to use it.

```lisp
(make-navigation 
   "navigation-name"
   (list "Nav item 1" "Widget 1 itself" nil)
   (list "Nav item 2" "Widget 2 itself" "some-url-for-future")
   ; ... some other navigation items
   )

## Using flashes

It is better to use ```'weblocks-twitter-bootstrap-application:bootstrap-flash``` instead of ```'weblocks:flash```
It has working close button.

## Assets dependencies

Package uses weblocks assets so all dependencies should be installed automatically.
