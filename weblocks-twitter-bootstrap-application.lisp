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
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*)
  (push (hunchentoot:create-static-file-dispatcher-and-handler 
          "/pub/stylesheets/twitter-bootstrap.css"
          (merge-pathnames 
            "twitter-bootstrap.css"
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*)
  (let ((empty-css-action 
          (lambda (&rest args)
            (setf (hunchentoot::header-out :content-type) "text/css")
            nil)))
    (push (hunchentoot:create-regex-dispatcher 
            "^/pub/stylesheets/dataform-import.css$" 
            empty-css-action) weblocks::*dispatch-table*) 
    (push (hunchentoot:create-regex-dispatcher 
            "^/pub/stylesheets/dataseq.css$" 
            empty-css-action) weblocks::*dispatch-table*)
    (push (hunchentoot:create-regex-dispatcher 
            "^/pub/stylesheets/datagrid.css$" 
            empty-css-action) weblocks::*dispatch-table*)
    (push (hunchentoot:create-regex-dispatcher 
            "^/pub/stylesheets/table.css$" 
            empty-css-action) weblocks::*dispatch-table*)))

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
    <link href=\"/pub/stylesheets/twitter-bootstrap.css\" rel=\"stylesheet\" media=\"screen\">
    <script src=\"http://code.jquery.com/jquery-latest.js\"></script>
    <script src=\"/pub/scripts/twitter-bootstrap-dialog.js\"></script>
    <script src=\"/bootstrap/js/bootstrap.min.js\"></script>
    {{{header-content}}}
  </head>
  <body>
    {{{body-content}}}
  </body>
</html>")

; Copied from weblocks/src/page-template.lisp
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

; Copied from weblocks/src/page-template.lisp
(defmethod render-page-body ((app twitter-bootstrap-webapp) body-string)
  (with-html
    (:div :class "page-wrapper container"
	  (render-extra-tags "page-extra-top-" 3)
	  (cl-who:htm (str body-string))
	  (render-extra-tags "page-extra-bottom-" 3))))

; Copied from weblocks/src/utils/html.lisp
(defmacro with-html-form ((method-type action &key id class enctype (use-ajax-p t) extra-submit-code
                          (submit-fn "initiateFormAction(\"~A\", $(this), \"~A\")")) &body body)
  "Transforms to cl-who (:form) with standard form code (AJAX support, actions, etc.)"
  (let ((action-code (gensym)))
    `(let ((,action-code (function-or-action->action ,action)))
       (with-html
         (:form :id ,id :class ,class :action (request-uri-path)
                :method (attributize-name ,method-type) :enctype ,enctype
                :onsubmit (when ,use-ajax-p
                            (format nil "~@[~A~]~A; return false;"
				    ,extra-submit-code
                                    (format nil ,submit-fn
                                            (hunchentoot:url-encode (or ,action-code ""))
                                            (session-name-string-pair))))
                (with-extra-tags
                  (cl-who:htm (:fieldset
                        ,@body
                        (:input :name weblocks::*action-string* :type "hidden" :value ,action-code))))))
       (log-form ,action-code :id ,id :class ,class))))

; Copied from weblocks/src/views/formview/formview.lisp
(defmethod with-view-header ((view form-view) obj widget body-fn &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     validation-errors
			     &allow-other-keys)
  (declare (special *on-ajax-complete-scripts* *form-submit-dependencies*))
  (let ((form-id (gen-id))
	(header-class (format nil "view form form-horizontal ~A"
			      (attributize-name (object-class-name obj)))))
    (when (>= (count-view-fields view)
	      (form-view-error-summary-threshold view))
      (setf header-class (concatenate 'string header-class " long-form")))
    (let ((form-body
	   (let ((*weblocks-output-stream* (make-string-output-stream)))
	     (with-html
               (arnesi:awhen (view-caption view)
                 (cl-who:htm (:h1 (cl-who:fmt (view-caption view) (humanize-name (object-class-name obj))))))
	       (render-validation-summary view obj widget validation-errors)
	       (safe-apply fields-prefix-fn view obj args)
	       (apply body-fn view obj args)
	       (safe-apply fields-suffix-fn view obj args)
	       (apply #'render-form-view-buttons view obj widget args)
	       (get-output-stream-string *weblocks-output-stream*)))))
      (with-html-form (method action
			      :id (when (form-view-focus-p view) form-id)
			      :class header-class
			      :enctype (form-view-default-enctype view)
			      :extra-submit-code (render-form-submit-dependencies *form-submit-dependencies*)
			      :use-ajax-p (form-view-use-ajax-p view))
	(write-string form-body *weblocks-output-stream*)))
    (when (form-view-focus-p view)
        (send-script (ps* `((@ ($ ,form-id) focus-first-element)))))))

; Copied from weblocks/src/views/formview/formview.lisp
(defmethod render-view-field ((field form-view-field) (view form-view)
			      widget presentation value obj 
			      &rest args &key validation-errors field-info &allow-other-keys)
  (declare (special *presentation-dom-id*))
  (let* ((attributized-slot-name (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name (view-field-slot-name field))))
	 (validation-error (assoc field validation-errors))
	 (field-class (concatenate 'string (arnesi:aif attributized-slot-name attributized-slot-name "")
				   (when validation-error " item-not-validated") " control-group"))
         (*presentation-dom-id* (gen-id)))
    (with-html
      (:div :class field-class
       (:label :class (format nil 
                              "~A control-label"(attributize-presentation
                                                  (view-field-presentation field)))
               :for *presentation-dom-id*
               (:span :class "slot-name"
                (:span :class "extra"
                 (unless (cl-containers:empty-p (view-field-label field))
                   (str (view-field-label field))
                   (str ":&nbsp;"))
                 (let ((required-indicator (weblocks::form-view-field-required-indicator field)))
                   (when (and (form-view-field-required-p field)
                              required-indicator)
                     (htm (:em :class "required-slot text-warning"
                           (if (eq t required-indicator)
                             (str *default-required-indicator*)
                             (str required-indicator))
                           (str "&nbsp;"))))))))
       (:div :class (format nil "control-group ~A" (when validation-error "warning"))
        (apply #'render-view-field-value
               value presentation
               field view widget obj
               :field-info field-info
               args)
        (when validation-error
          (htm (:p :class "help-inline"
                (str (format nil "~A" (cdr validation-error)))))))))))

; Copied from weblocks/src/views/formview/formview.lisp
(defmethod render-form-view-buttons ((view form-view) obj widget &rest args &key form-view-buttons &allow-other-keys)
    (declare (ignore obj args))
    (flet ((find-button (name)
	     (arnesi:ensure-list
	       (if form-view-buttons
		   (find name form-view-buttons
			 :key (lambda (item)
				(car (arnesi:ensure-list item))))
		 (find name (form-view-buttons view)
		       :key (lambda (item)
			      (car (arnesi:ensure-list item))))))))
      (with-html
        (:div :class "submit control-group"
          (:div :class "controls"
           (let ((submit (find-button :submit)))
             (when submit
               (render-button *submit-control-name*
                              :value (or (cdr submit)
                                         (humanize-name (car submit)))
                              :class "submit btn btn-primary")))
           (str "&nbsp;")
           (let ((cancel (find-button :cancel)))
             (when cancel
               (render-button *cancel-control-name*
                              :class "btn submit cancel"
                              :value (or (cdr cancel)
                                         (humanize-name (car cancel)))))))))))

; Copied from weblocks/src/views/formview/formview.lisp
(defmethod render-validation-summary ((view form-view) obj widget errors)
  (declare (ignore view obj))
  (when errors
    (let ((non-field-errors (weblocks::find-all errors #'null :key #'car))
          (field-errors (weblocks::find-all errors (arnesi:compose #'not #'null) :key #'car)))
      (with-html
        (:div :class "text-error"
         (let ((error-count (length errors)))
           (htm 
             (:h4 (if (eql error-count 1)
                    (str (format nil "There is 1 validation error:"))
                    (str (format nil "There are ~S validation errors:" error-count))))))
         (when non-field-errors
           (htm
             (:ul :class "non-field-validation-errors"
              (mapc (lambda (err)
                      (with-html
                        (:li
                          (str (format nil "~A" (cdr err))))))
                    non-field-errors))))
         (when field-errors
           (htm
             (:ul :class "field-validation-errors"
              (mapc (lambda (err)
                      (with-html
                        (:li
                          (str (format nil "~A" (cdr err))))))
                    field-errors)))))))))

; Copied from weblocks/src/widgets/datagrid/sort.lisp
(defmethod render-view-field-header ((field table-view-field) (view table-view)
				     (widget datagrid) presentation value obj 
				     &rest args &key field-info
				     &allow-other-keys)
  (declare (ignore args))
  (if (dataseq-field-sortable-p widget field)
    (let* ((slot-name (view-field-slot-name field))
           (slot-path (get-field-info-sort-path field-info))
           (th-class (when (equalp slot-path (dataseq-sort-path widget))
                       (concatenate 'string " sort-"
                                    (attributize-name (string (dataseq-sort-direction widget))))))
           (sort-dir (dataseq-sort-direction widget)))
      (with-html
        (:th :class (concatenate 'string
                                 (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name slot-name))
                                 th-class)
             (render-link
               (make-action
                 (lambda (&rest args)
                   (declare (ignore args))
                   (let ((new-dir :asc))
                     (when (equalp (dataseq-sort-path widget) slot-path)
                       (setf new-dir (negate-sort-direction sort-dir)))
                     (setf (dataseq-sort widget) (cons slot-path new-dir)))
                   ;; we also need to clear the selection
                   (dataseq-clear-selection widget)))
               
               (with-html-output-to-string (s)
                 (:span (str (view-field-label field))
                  (str "&nbsp;"))
                 (if (equalp slot-path (dataseq-sort-path widget))
                   (htm (:i :class (if (equal (dataseq-sort-direction widget) :asc) "icon-chevron-up" "icon-chevron-down")))
                   (htm (:i :class "icon-white"))) 
                 s)
               :class "nowrap"))))
      (call-next-method)))

; Copied from weblocks/src/widgets/dataseq/dataseq.lisp
(defmethod dataseq-render-operations-default ((obj dataseq) &rest args)
  (declare (ignore args))
  (flet ((render-operations ()
           (with-html
             (:div :class "operations pull-right btn-group"
              (mapc (lambda (op)
                      (render-button (car op) :class "submit btn"))
                    (append
                      (dataseq-common-ops obj)
                      (when (dataseq-allow-select-p obj)
                        (dataseq-item-ops obj))))))))
    (if (dataseq-wrap-body-in-form-p obj)
      (render-operations)
      (with-html-form (:get (make-action (curry #'dataseq-operations-action obj))
                       :class "operations-form")
                      (render-operations)))))

; Copied from weblocks/src/views/tableview.lisp
(defmethod with-table-view-header ((view table-view) obj widget header-fn rows-fn &rest args
                                                     &key summary &allow-other-keys)
  (with-html
    (:table :class "table-striped table-bordered" :summary (or summary (table-view-default-summary view))
     (when (view-caption view)
       (htm (:caption (str (view-caption view)))))
     (htm
       (:thead
         (apply header-fn view (car obj) widget args))
       (:tbody
         (apply rows-fn view obj widget args))))))

; Copied from weblocks/src/widgets/datagrid/drilldown.lisp
(defmethod with-table-view-body-row ((view table-view) obj (widget datagrid) &rest args
				     &key alternp &allow-other-keys)
  (if (and (dataseq-allow-drilldown-p widget)
	   (dataseq-on-drilldown widget))
      (let ((row-action (make-action
			 (lambda (&rest args)
			   (declare (ignore args))
			   (when (dataseq-autoset-drilled-down-item-p widget)
			     (setf (dataseq-drilled-down-item widget) obj))
			   (funcall (cdr (dataseq-on-drilldown widget)) widget obj))))
	    (drilled-down-p (and (dataseq-drilled-down-item widget)
				 (eql (object-id (dataseq-drilled-down-item widget))
				      (object-id obj)))))
	(safe-apply (sequence-view-row-prefix-fn view) view obj args)
	(with-html
	  (:tr :class (when (or alternp drilled-down-p)
			(concatenate 'string
				     (when alternp "altern")
				     (when (and alternp drilled-down-p) " ")
				     (when drilled-down-p "drilled-down info")))
	       :onclick (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");"
				row-action (session-name-string-pair))
	       :onmouseover "this.style.cursor = \"pointer\";"
	       :style "cursor: expression(\"hand\");"
	       (apply #'render-table-view-body-row view obj widget :row-action row-action args)))
	(safe-apply (sequence-view-row-suffix-fn view) view obj args))
      (call-next-method)))

(in-package :weblocks)

(defun render-message (message &optional caption)
  "Renders a message to the user with standardized markup."
  (with-html
    (:p :class "user-message well"
        (when caption
          (htm (:span :class "caption" (str caption) ":&nbsp;")))
	(:span :class "message" (str message)))))

(defun render-button (name  &key (value (humanize-name name)) id (class "submit btn"))
  "Renders a button in a form.

'name' - name of the html control. The name is attributized before
being rendered.
'value' - a value on html control. Humanized name is default.
'id' - id of the html control. Default is nil.
'class' - a class used for styling. By default, \"submit\"."
  (with-html
    (:input :name (attributize-name name) :type "submit" :id id :class class
	    :value value :onclick "disableIrrelevantButtons(this);")
    (str "&nbsp;")))
