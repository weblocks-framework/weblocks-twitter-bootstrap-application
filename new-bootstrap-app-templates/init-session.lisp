
(in-package :{APPNAME})

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
  (list (lambda (&rest args)
    (with-html
      (:div :class "container"
            (:h1 :align "center" "Happy Hacking!")))))))

