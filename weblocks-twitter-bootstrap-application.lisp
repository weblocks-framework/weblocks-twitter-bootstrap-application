;;;; weblocks-twitter-bootstrap-application.lisp

(in-package #:weblocks-twitter-bootstrap-application)

(defclass twitter-bootstrap-webapp (weblocks:weblocks-webapp)
  ()
  (:metaclass weblocks:webapp-class))

(defmethod initialize-webapp :before ((app twitter-bootstrap-webapp))
  (push (hunchentoot:create-folder-dispatcher-and-handler 
          "/bootstrap/" 
          (merge-pathnames 
            (make-pathname :directory '(:relative "bootstrap-compiled"))
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*)
  (push (hunchentoot:create-static-file-dispatcher-and-handler 
          "/pub/scripts/twitter-bootstrap-dialog.js"
          (merge-pathnames 
            "twitter-bootstrap-dialog.js"
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*))

(defmethod initialize-instance :after ((self twitter-bootstrap-webapp) &key ignore-default-dependencies &allow-other-keys)
  (unless ignore-default-dependencies
    (setf (weblocks::weblocks-webapp-application-dependencies self)
          '((:script "jquery-seq")
            (:script "weblocks-jquery")))))

(defmacro capture-weblocks-output (&body body)
  `(let ((*weblocks-output-stream* (make-string-output-stream)))
     ,@body 
     (get-output-stream-string *weblocks-output-stream*)))

(mustache:defmustache twitter-bootstrap-template 
"<!DOCTYPE html>
<html>
  <head>
    <title>{{title}}</title>
    <!-- Bootstrap -->
    <style type=\"text/css\">
#ajax-progress
{
  position: fixed;
  right: 0;
  top: 0;
  z-index: 1000;
}

/* IE 6 specific progress bar fix */
* html #ajax-progress
{
  position: absolute;
  left: expression((documentElement.scrollLeft
                     + (documentElement.clientWidth - this.clientWidth))
                   + 'px')
  top: expression(documentElement.scrollTop + 'px');
}
</style>
    <link href=\"/bootstrap/css/bootstrap.min.css\" rel=\"stylesheet\" media=\"screen\">
    <script src=\"http://code.jquery.com/jquery-latest.js\"></script>
    <script src=\"/pub/scripts/twitter-bootstrap-dialog.js\"></script>
    <script src=\"/bootstrap/js/bootstrap.min.js\"></script>
    {{{header-content}}}
  </head>
  <body>
    {{{body-content}}}
  </body>
</html>")

(defmethod render-page ((app twitter-bootstrap-webapp))
  (declare (special weblocks::*page-dependencies*))
  (let* ((rendered-html (get-output-stream-string weblocks:*weblocks-output-stream*))
         (all-dependencies (timing "compact-dependencies"
                                   (compact-dependencies (append (webapp-application-dependencies)
                                                                 weblocks::*page-dependencies*))))
         (header-content (capture-weblocks-output 
                           (render-page-headers app)
                           (mapc #'render-dependency-in-page-head all-dependencies)))
         (body-content (capture-weblocks-output 
                         (render-page-body app rendered-html)
                         (with-html 
                           (:div :id "ajax-progress" "&nbsp;")
                           (with-javascript "updateWidgetStateFromHash();")))))
    (let ((mustache:*mustache-output* weblocks:*weblocks-output-stream*))
      (twitter-bootstrap-template 
        `((:title . ,(application-page-title app))
          (:header-content . ,header-content)
          (:body-content . ,body-content))))))

(defmethod render-page-body ((app twitter-bootstrap-webapp) body-string)
  (with-html
    (:div :class "page-wrapper container"
	  (render-extra-tags "page-extra-top-" 3)
	  (cl-who:htm (str body-string))
	  (render-extra-tags "page-extra-bottom-" 3))))
