(in-package :weblocks-twitter-bootstrap-application)

(defwidget bootstrap-navbar-navigation (navigation)
  ())
 
(defun render-bootstrap-navbar-menu-wt (&rest args &key menu-id menu-header menu-content menu-empty-p menu-empty-message menu-ordered-list-p)
  (with-html-to-string
    (:div :class "navbar" 
     :id menu-id
     (:div :class "navbar-inner"
      (when menu-header
        (htm (:h1 (str menu-header))))
      (:ul :class "nav" (str menu-content))))))
 
(defun render-bootstrap-navbar-menu-item-wt (&rest args &key item-id pane-class span-class content target-is-function pane-selected-or-disabled item-href)
  (with-html-to-string
    (:li :id item-id :class (if pane-selected-or-disabled "active")
     (if target-is-function 
       (cl-who:htm (str content))
       (cl-who:htm (:a :href item-href (str content)))))))
 
(defmethod render-navigation-menu :around ((obj bootstrap-navbar-navigation) &rest args &key menu-args &allow-other-keys)
  (declare (ignore args))
  (apply #'render-menu (navigation-menu-items obj)
         :base (selector-base-uri obj)
         :selected-pane (static-selector-current-pane obj)
         :header (navigation-header obj)
         :container-id (dom-id obj)
         :empty-message "No navigation entries"
         :menu-template #'render-bootstrap-navbar-menu-wt
         :item-template  #'render-bootstrap-navbar-menu-item-wt
         menu-args))
