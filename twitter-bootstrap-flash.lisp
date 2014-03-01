(in-package :weblocks-twitter-bootstrap-application)

(defwidget bootstrap-flash (flash)
           ())

(defmethod render-widget-body ((obj bootstrap-flash) &rest args)
  (declare (special *on-ajax-complete-scripts* *dirty-widgets*))
  (let ((messages (weblocks::flash-messages-to-show obj)))
    (when messages
      (write-string 
        (render-template-to-string 
          :flash-messages-wt 
          (list :widget obj)
          :content (format 
                     nil "~{~A~}"
                     (mapcar 
                       (lambda (msg)
                         (render-template-to-string 
                           :single-bootstrap-flash-message-wt
                           (list :widget msg)
                           :content (capture-weblocks-output 
                                      (apply #'render-widget msg args))
                           :close-onclick (ps:ps 
                                            (initiate-action 
                                              (ps:LISP 
                                                (function-or-action->action 
                                                  (lambda (&rest args)
                                                    (setf (weblocks::flash-old-messages obj)  
                                                          (remove-if (lambda (message)
                                                                       (equal msg message))
                                                                     (weblocks::flash-old-messages obj)))
                                                    (mark-dirty obj))))
                                              (ps:LISP (session-name-string-pair))))))
                       messages)))
        *weblocks-output-stream*)
      (send-script (ps* `((@ ($ ,(dom-id obj)) show)))))))

(defun single-bootstrap-flash-message-wt (&key content close-onclick)
  (yaclml:with-yaclml-output-to-string 
    (<:div :class "alert" 
           (<:as-is 
             (format nil "<button type=\"button\" class=\"close\" data-dismiss=\"alert\" onclick=\"~A\">x</button>" close-onclick))
           ;(<:button :type "button" :class "close" :data-dismiss "alert" "x" :onclick close-onclick)
           (<:as-is content))))

(deftemplate :single-bootstrap-flash-message-wt 'single-bootstrap-flash-message-wt 
             :application-class 'twitter-bootstrap-webapp)
