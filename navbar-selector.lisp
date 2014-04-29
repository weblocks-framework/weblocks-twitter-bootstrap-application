(in-package :weblocks-twitter-bootstrap-application)

(defwidget bootstrap-navbar-selector (simple-selector)
  ((selected :accessor bootstrap-navbar-selector-selected)
   (titles :initarg :titles)))

(defmethod get-selector-link (widget selected)
  ; (url-for :navbar.selected j)
  (make-action-url 
    (function-or-action->action 
      (lambda (&rest args)
        (setf (slot-value widget 'selected) selected)
        (mark-dirty widget)))))

(defmethod render-widget-body ((obj bootstrap-navbar-selector) &rest args)
  (unless (slot-value obj 'selected)
    (setf (slot-value obj 'selected) (first (slot-value obj 'names))))

  (render-wt 
    :bootstrap-navbar-menu-wt 
    (list :widget obj :selected (slot-value obj 'selected))
    :menu-id "navbar-nav"
    :menu-content (arnesi:join-strings 
                    (loop for i in (slot-value obj 'titles) 
                          for j in (slot-value obj 'names)
                          for k from 0
                          collect (render-wt-to-string 
                                    :bootstrap-navbar-menu-item-wt 
                                    (list :widget obj)
                                    :pane-selected-or-disabled (string= j (slot-value obj 'selected))
                                    :item-href (get-selector-link obj j)
                                    :content i)))))

(defun make-navbar-selector (name &rest args)
  (let ((names  (mapcar #'third args)))
    (when names
      (setf (car names) ""))
    (make-instance 'bootstrap-navbar-selector 
                   :names names
                   :titles (mapcar #'first args)
                   :child-callbacks (mapcar (lambda (item) (lambda () item)) (mapcar #'second args))
                   :uri-id (alexandria:make-keyword (string-upcase name))
                   :public-parameters '(:selected))))
