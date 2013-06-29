;;;; weblocks-twitter-bootstrap-application.lisp

(in-package #:weblocks-twitter-bootstrap-application)

(defclass twitter-bootstrap-webapp (weblocks:weblocks-webapp)
  ((responsiveness-enabled-p :initform t :initarg :responsiveness-enabled-p))
  (:metaclass weblocks:webapp-class))

; +weblocks-normal-theme-compatible
(defmethod initialize-webapp :before ((app twitter-bootstrap-webapp))
  (flet ((prepend-webapp-path (value)
           (format nil "~A~A" (string-right-trim "/" (weblocks::weblocks-webapp-prefix app)) value)))
    (push (weblocks:create-folder-dispatcher-and-handler 
            (prepend-webapp-path "/bootstrap/") 
            (merge-pathnames 
              (make-pathname :directory '(:relative "bootstrap-compiled"))
              (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
    (push (weblocks:create-static-file-dispatcher-and-handler 
            (prepend-webapp-path "/pub/scripts/twitter-bootstrap-dialog.js")
            (merge-pathnames 
              "twitter-bootstrap-dialog.js"
              (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
    (push (weblocks:create-static-file-dispatcher-and-handler 
            (prepend-webapp-path "/pub/stylesheets/twitter-bootstrap.css")
            (merge-pathnames 
              "twitter-bootstrap.css"
              (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
    (push (weblocks:create-static-file-dispatcher-and-handler 
            (prepend-webapp-path "/pub/scripts/jquery-1.8.2.js")
            (merge-pathnames 
              "jquery-1.8.2.js"
              (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
    (push (weblocks:create-static-file-dispatcher-and-handler 
            (prepend-webapp-path "/pub/scripts/datagrid.js")
            (merge-pathnames 
              "datagrid.js"
              (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*)) 

  (let ((empty-css-action 
          (lambda (&rest args)
            (setf (hunchentoot::header-out :content-type) "text/css")
            nil)))
    (flet ((add-empty-css-action (regex)
             (setf regex (format nil "^~A~A$" (string-right-trim "/" (weblocks::weblocks-webapp-prefix app)) regex))
             (push (hunchentoot:create-regex-dispatcher 
                     regex
                     empty-css-action) weblocks::*dispatch-table*)))
      (add-empty-css-action "/pub/stylesheets/dataform-import.css")
      (add-empty-css-action "/pub/stylesheets/pagination.css")
      (add-empty-css-action "/pub/stylesheets/flash.css")
      (add-empty-css-action "/pub/stylesheets/dataseq.css")
      (add-empty-css-action "/pub/stylesheets/datagrid.css")
      (add-empty-css-action "/pub/stylesheets/table.css"))))

; +weblocks-normal-theme-compatible +not-tested
(defmethod initialize-instance :after ((self twitter-bootstrap-webapp) &key ignore-default-dependencies &allow-other-keys)
  (unless ignore-default-dependencies
    (setf (weblocks::weblocks-webapp-application-dependencies self)
          '((:script "jquery-seq" :default t)
            (:script "weblocks-jquery" :default t)))))

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
  z-index: 10000000;
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
    <link href=\"{{webapp-files-prefix}}/bootstrap/css/bootstrap.min.css\" rel=\"stylesheet\" media=\"screen\">
    <link href=\"{{webapp-files-prefix}}/pub/stylesheets/twitter-bootstrap.css\" rel=\"stylesheet\" media=\"screen\">
    <script src=\"{{webapp-files-prefix}}/pub/scripts/jquery-1.8.2.js\"></script>
    <script src=\"{{webapp-files-prefix}}/pub/scripts/twitter-bootstrap-dialog.js\"></script>
    <script src=\"{{webapp-files-prefix}}/bootstrap/js/bootstrap.min.js\"></script>
    {{#responsiveness-enabled-p}}
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <link href=\"{{webapp-files-prefix}}/bootstrap/css/bootstrap-responsive.css\" rel=\"stylesheet\">
    {{/responsiveness-enabled-p}}
    {{{header-content}}}
  </head>
  <body>
    {{{body-content}}}
  </body>
</html>")

; Copied from weblocks/src/page-template.lisp
; +weblocks-normal-theme-compatible +not-tested
(defmethod render-page ((app twitter-bootstrap-webapp))
  (declare (special weblocks::*page-dependencies*))
  (setf (weblocks::weblocks-webapp-application-dependencies app)
        (loop for i in (weblocks::weblocks-webapp-application-dependencies app) 
              collect (if (and 
                            (listp i)
                            (getf i :default))
                        (make-local-dependency (first i) (second i) :do-not-probe t)
                        i)))
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
                           (with-javascript "updateWidgetStateFromHash();"))))
         (webapp-files-prefix (string-right-trim "/" (weblocks::weblocks-webapp-prefix app))))

    (let ((mustache:*mustache-output* weblocks:*weblocks-output-stream*))
      (twitter-bootstrap-template 
        `((:title . ,(application-page-title app))
          (:header-content . ,header-content)
          (:body-content . ,body-content)
          (:webapp-files-prefix . ,webapp-files-prefix)
          (:responsiveness-enabled-p . ,(slot-value app 'responsiveness-enabled-p)))))))

; Copied from weblocks/src/page-template.lisp
; +weblocks-normal-theme-compatible +not-tested
(defmethod render-page-body ((app twitter-bootstrap-webapp) body-string)
  (with-html
    (:div :class "page-wrapper container"
	  (render-extra-tags "page-extra-top-" 3)
	  (cl-who:htm (str body-string))
	  (render-extra-tags "page-extra-bottom-" 3))))

(defun in-bootstrap-application-p ()
  (subtypep (class-of (current-webapp)) 'twitter-bootstrap-webapp))


(defmacro return-normal-value-when-theme-not-used (function)
  `(unless (in-bootstrap-application-p)
     (return-from ,function (call-next-method))))

; Copied from weblocks/src/views/formview/formview.lisp
; +weblocks-normal-theme-compatible
(defmethod with-view-header :around ((view form-view) obj widget body-fn &rest args &key
			     (method (form-view-default-method view))
			     (action (form-view-default-action view))
			     (fields-prefix-fn (view-fields-default-prefix-fn view))
			     (fields-suffix-fn (view-fields-default-suffix-fn view))
			     validation-errors
			     &allow-other-keys)
  (declare (special *on-ajax-complete-scripts* *form-submit-dependencies*))

  (return-normal-value-when-theme-not-used with-view-header)

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
                 (cl-who:htm 
                   (:h1 (cl-who:fmt (view-caption view) (humanize-name (object-class-name obj))))
                   (:hr)))
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

(defun form-view-field-wt (&key label-class id show-required-indicator required-indicator-label 
                                show-field-label field-label validation-error content 
                                field-class)
  (with-html-to-string
    (:div :class (format nil "control-group ~A" field-class)
     (:label :class (format nil "~A control-label"label-class)
      :for id
      (:span :class "slot-name"
       (:span :class "extra"
        (when show-field-label
          (str field-label)
          (str ":&nbsp;"))
        (when show-required-indicator
          (htm (:em :class "required-slot text-warning"
                (str required-indicator-label)
                (str "&nbsp;")))))))
     (:div :class (format nil "controls ~A" (when validation-error "warning"))
      (str content)
      (when validation-error
        (htm (:p :class "help-inline"
              (str validation-error))))))))

(deftemplate :form-view-field-wt 'form-view-field-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun form-view-field-checkbox-presentation-wt (&key label-class id show-required-indicator required-indicator-label 
                                show-field-label field-label validation-error content 
                                field-class)
  (with-html-to-string
    (:div :class (format nil "control-group ~A" field-class)
     (:label :class (format nil "~A-presentation control-label"label-class)
      :for id
      (:span :class "slot-name"
       (:span :class "extra"
        (when show-field-label
          (str field-label)
          (str ":&nbsp;"))
        (when show-required-indicator
          (htm (:em :class "required-slot text-warning"
                (str required-indicator-label)
                (str "&nbsp;")))))))
     (:div :class (format nil "controls ~A" (when validation-error "warning"))
      (str content)
      (when validation-error
        (htm (:p :class "help-inline"
              (str validation-error))))))))

(deftemplate :form-view-field-wt 'form-view-field-checkbox-presentation-wt 
             :context-matches (lambda (&key presentation &allow-other-keys)
                                (if (typep presentation 'weblocks:checkbox-presentation)
                                  10 
                                  0))
             :application-class 'twitter-bootstrap-webapp)


(defun form-view-buttons-wt (&key submit-html cancel-html)
  (with-html-to-string
    (:div :class "submit control-group"
     (:div :class "controls"
      (when submit-html 
        (str submit-html))
      (str "&nbsp;")
      (when cancel-html 
        (str cancel-html))))))

(deftemplate :form-view-buttons-wt 'form-view-buttons-wt 
             :application-class 'twitter-bootstrap-webapp)


(defun table-view-field-header-wt (&key row-class label)
  (with-html-to-string
    (:th :class row-class
     (:span (str label)))))

(deftemplate :table-view-field-header-wt 
             'table-view-field-header-wt 
             :application-class 'twitter-bootstrap-webapp)

; Copied from weblocks/src/views/formview/formview.lisp
; +weblocks-normal-theme-compatible +not-tested
(defmethod render-validation-summary :around ((view form-view) obj widget errors)
  (declare (ignore view obj))

  (return-normal-value-when-theme-not-used render-validation-summary)

  (when errors
    (let ((non-field-errors (weblocks::find-all errors #'null :key #'car))
          (field-errors (weblocks::find-all errors (arnesi:compose #'not #'null) :key #'car)))
      (with-html
        (:div :class "text-error"
         (let ((error-count (length errors)))
           (htm 
             (:h4 (cl-who:fmt (proper-number-form error-count "There is ~S validation error:") error-count))))
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
; +weblocks-normal-theme-compatible +not-tested
(defmethod render-view-field-header :around ((field table-view-field) (view table-view)
				     (widget datagrid) presentation value obj 
				     &rest args &key field-info
				     &allow-other-keys)
  (declare (ignore args))

  (return-normal-value-when-theme-not-used render-view-field-header)

  (if (dataseq-field-sortable-p widget field)
    (let* ((slot-path (get-field-info-sort-path field-info))
           (th-class (when (equalp slot-path (dataseq-sort-path widget))
                       (concatenate 'string " sort-"
                                    (attributize-name (string (dataseq-sort-direction widget))))))
           (sort-dir (dataseq-sort-direction widget)))
      (with-html
        (:th :class (concatenate 'string
                                 (if field-info
                                   (attributize-view-field-name field-info)
                                   (attributize-name (view-field-slot-name field)))
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
               
               (weblocks:with-html-to-string
                 (:span (str (weblocks-util:translate (view-field-label field)))
                  (str "&nbsp;"))
                 (if (equalp slot-path (dataseq-sort-path widget))
                   (htm (:i :class (if (equal (dataseq-sort-direction widget) :asc) "icon-chevron-up" "icon-chevron-down")))
                   (htm (:i :class "icon-white"))))
               :class "nowrap"))))
      (call-next-method)))

; Copied from weblocks/src/widgets/dataseq/dataseq.lisp
; +weblocks-normal-theme-compatible +not-tested
(defmethod dataseq-render-operations-default :around ((obj dataseq) &rest args)
  (declare (ignore args))

  (return-normal-value-when-theme-not-used dataseq-render-operations-default)

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
; +weblocks-normal-theme-compatible +not-tested
(defun bootstrap-striped-bordered-table-view-header-wt (&key caption summary header-content content)
  (with-html-to-string
    (:table :class "table-striped table-bordered" :summary summary 
     (when caption
       (htm (:caption (str caption))))
     (htm
       (:thead
         (str header-content))
       (:tbody
         (str content))))))

(deftemplate :table-view-header-wt 
             'bootstrap-striped-bordered-table-view-header-wt 
             :application-class 'twitter-bootstrap-webapp)

; Copied from weblocks/src/widgets/datagrid/drilldown.lisp
; +weblocks-normal-theme-compatible +not-tested
(defmethod with-table-view-body-row :around ((view table-view) obj (widget datagrid) &rest args
				     &key alternp &allow-other-keys)

  (return-normal-value-when-theme-not-used with-table-view-body-row)

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

(defun button-wt (&key value name id class disabledp submitp)
  (with-html-to-string
    (:input :name name :type "submit" :id id :class (format nil "btn ~A ~A" class (when submitp "btn-primary"))
     :value value :disabled (when disabledp "disabled")
     :onclick "disableIrrelevantButtons(this);")))

(deftemplate :button-wt 'button-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun checkboxes-view-field-wt (&key field-class content field-label show-required-indicator required-indicator-label validation-error)
  (with-html-to-string
    (:div :class (format nil "control-group ~A" field-class)
     (:label :class "control-label"
      (:span :class "slot-name"
       (:span :class "extra"
        (str field-label) ":&nbsp;"
        (when show-required-indicator
          (htm (:em :class "required-slot text-warning" (str required-indicator-label)))))))
     (:div :class (format nil "controls ~A" (when validation-error "warning"))
      (str content) 
      (when validation-error
        (htm (:p :class "validation-error"
              (:em
                (:span :class "validation-error-heading")
                (str validation-error)))))))))

(deftemplate :checkboxes-view-field-wt 'checkboxes-view-field-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun modal-wt (&key title content css-class)
  (with-html-to-string
    (:div :class "modal-backdrop")
    (:div :class "modal"
     (:h1 (:span (str title)))
     (:div :class css-class
      (str content)))))

(deftemplate :modal-wt 'modal-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun render-message-wt (&key message caption)
  "Renders a message to the user with standardized markup."
  (with-html-to-string
    (:p :class "user-message well"
     (when caption
       (htm (:span :class "caption" (str caption) ":&nbsp;")))
     (:span :class "message" (str message)))))

(deftemplate :render-message-wt 'render-message-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun single-flash-message-wt (&key content)
  (with-html-to-string 
    (:div :class "alert" 
     (:button :type "button" :class "close" :data-dismiss "alert" "x")
     (str content))))

(deftemplate :single-flash-message-wt 'single-flash-message-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun flash-messages-wt (&key content)
  (with-html-to-string 
    (:div :class "view"
     (with-extra-tags
       (htm
         (:div :class "messages clearfix"
          (str content)))))))

(deftemplate :flash-messages-wt 'flash-messages-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun table-view-field-header-value-wt (&key content)
  (with-html-to-string
    (:span :class "label-bootstrap" (str content))))

(deftemplate :table-view-field-header-value-wt 'table-view-field-header-value-wt 
             :application-class 'twitter-bootstrap-webapp)

(in-package :weblocks)

; +weblocks-normal-theme-compatible +not-tested
(defmethod render-widget-body :around ((obj pagination) &rest args) 
  (declare (ignore args)
           (special *request-hook*))

  (weblocks-twitter-bootstrap-application::return-normal-value-when-theme-not-used render-widget-body)

  (when (> (pagination-page-count obj) 0)
    (with-html 
      (:div :class "pagination-inner"
      ; 'Previous' link
      (:div :class "pull-left"
       (when (> (pagination-current-page obj) 1)
	(render-link (lambda (&rest args)
                       (declare (ignore args))
		       (when (> (pagination-current-page obj) 1)
			 (decf (pagination-current-page obj))
			 (pagination-call-on-change obj)))
		     (humanize-name "< Previous")
		     :class "previous-page btn")
	(str "&nbsp;")))
      ; 'Viewing Page X of Y'
      (:span :class "page-info pull-left"
	     (:span :class "viewing-label" (str (translate "Viewing ")))
	     (:span :class "page-label" (str (translate "Page ")))
	     (:span :class "current-page" (:strong (str (pagination-current-page obj))))
	     (:span :class "of-label" (str (translate " of ")))
	     (:span :class "total-pages" (str (pagination-page-count obj))))
      ; 'Next' link
      (:div :class "pull-left"
       (when (< (pagination-current-page obj)
	       (pagination-page-count obj))
	(str "&nbsp;")
	(render-link (lambda (&rest args)
                       (declare (ignore args))
		       (when (< (pagination-current-page obj)
				(pagination-page-count obj))
			 (incf (pagination-current-page obj))
			 (pagination-call-on-change obj)))
		     (humanize-name "Next >")
		     :class "next-page btn")))
      ; Go to page
      (when (> (pagination-page-count obj) 1)
        (htm 
          (:div :class "pull-left"
           "&nbsp;")
          (:div :class "pull-left form-inline"
           (with-html-form (:get (curry #'pagination-on-go-to-page obj))
             (:div :class "pull-left"
              (:label (:span "Go to page:&nbsp;")
               (:input :name "page-number"
                :type "text"
                :class (concatenate 'string
                                    "page-number input-small"
                                    (when (slot-value obj 'last-request-error-p)
                                      " item-not-validated"))
                :onfocus (concatenate 'string
                                      "$(this).removeClassName(\"item-not-validated\");"
                                      (when (/= (pagination-current-page obj) 1)
                                        "if(this.value == \"1\") { this.value = \"\"; }"))
                :onblur (when (/= (pagination-current-page obj) 1)
                          "if(this.value == \"\") { this.value = \"1\"; }")
                :value (when (/= (pagination-current-page obj) 1)
                         "1"))))
             (:div :class "pull-left"
               (render-button "go-to-page" :value "Go"))))))
      ; Total items
      (pagination-render-total-item-count obj)))))

