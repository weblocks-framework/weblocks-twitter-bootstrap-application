;;;; weblocks-twitter-bootstrap-application.lisp

(in-package #:weblocks-twitter-bootstrap-application)

(defclass twitter-bootstrap-webapp (weblocks:weblocks-webapp)
  ((responsiveness-enabled-p :initform t :initarg :responsiveness-enabled-p))
  (:metaclass weblocks:webapp-class))

; +weblocks-normal-theme-compatible
(defmethod initialize-webapp :before ((app twitter-bootstrap-webapp))
  (weblocks-utils:require-assets 
    "https://raw.github.com/html/weblocks-assets/master/twitter-bootstrap/2.2.1/"
    :webapp app)
  (weblocks-utils:require-assets 
    "https://raw.github.com/html/weblocks-assets/master/jquery/1.8.2/"
    :webapp app)
  (weblocks-utils:require-assets 
    "https://raw.github.com/html/weblocks-assets/master/jquery-seq/0.0.1/"
    :webapp app)
  (weblocks-utils:require-assets 
    "https://raw.github.com/html/weblocks-assets/master/weblocks-jquery/0.1.4/"
    :webapp app)
  (push (weblocks:create-static-file-dispatcher-and-handler 
          (weblocks-utils:prepend-webapp-path "/pub/scripts/twitter-bootstrap-dialog.js" app)
          (merge-pathnames 
            "twitter-bootstrap-dialog.js"
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
  (push (weblocks:create-static-file-dispatcher-and-handler 
          (weblocks-utils:prepend-webapp-path "/pub/stylesheets/twitter-bootstrap.css" app)
          (merge-pathnames 
            "twitter-bootstrap.css"
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 
  (push (weblocks:create-static-file-dispatcher-and-handler 
          (weblocks-utils:prepend-webapp-path "/pub/scripts/datagrid.js" app)
          (merge-pathnames 
            "datagrid.js"
            (asdf-system-directory :weblocks-twitter-bootstrap-application))) weblocks::*dispatch-table*) 

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

(defmethod weblocks:weblocks-webapp-default-dependencies ((self twitter-bootstrap-webapp))
  `((:script "jquery-1.8.2" :default t)
    (:script "jquery-seq" :default t)
    (:script "weblocks-jquery" :default t)
    (:script "twitter-bootstrap-dialog" :default t)
    ,(make-instance 'script-dependency 
                    :url (make-instance 'puri:uri :path (weblocks-utils:prepend-webapp-path "/bootstrap/js/bootstrap.js" self)))))

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
    <link href=\"{{webapp-files-prefix}}/bootstrap/css/bootstrap.css\" rel=\"stylesheet\" media=\"screen\">
    <link href=\"{{webapp-files-prefix}}/pub/stylesheets/twitter-bootstrap.css\" rel=\"stylesheet\" media=\"screen\">
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

(defun form-view-body-wt (&key caption class-name validation-summary fields-prefix fields-suffix form-view-buttons content method action form-id header-class enctype extra-submit-code use-ajax-p)
  (with-html-to-string
    (with-html-form (method action
                            :id form-id
                            :class (format nil "form-horizontal ~A" header-class)
                            :enctype enctype
                            :extra-submit-code extra-submit-code
                            :use-ajax-p use-ajax-p)

      (when caption
        (htm (:h1 (fmt caption class-name))
             (:hr)))
      (str validation-summary)
      (str fields-prefix)
      (str content)
      (str fields-suffix)
      (str form-view-buttons))))

(deftemplate :form-view-body-wt 'form-view-body-wt 
             :application-class 'twitter-bootstrap-webapp)


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

(defun datagrid-table-view-body-row-wt (&key row-class prefix suffix row-action session-string content alternp drilled-down-p &allow-other-keys)
  (with-html-to-string
    (str prefix)
    (:tr :class (format nil "~A~A" row-class (if drilled-down-p " info" ""))
     :onclick (format nil "initiateActionOnEmptySelection(\"~A\", \"~A\");" row-action session-string)
     :onmouseover "this.style.cursor = \"pointer\";"
     :style "cursor: expression(\"hand\");"
     (str content))
    (str suffix)))

(deftemplate :datagrid-table-view-body-row-wt 'datagrid-table-view-body-row-wt 
             :application-class 'twitter-bootstrap-webapp)

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

(defun template-form-view-body-wt (&rest args &key caption class-name validation-summary form-view-buttons content method action form-id header-class enctype extra-submit-code use-ajax-p fields-data &allow-other-keys)
  (with-html-to-string
    (with-html-form (method action
                            :id form-id
                            :class (format nil "~A form-horizontal" header-class)
                            :enctype enctype
                            :extra-submit-code extra-submit-code
                            :use-ajax-p use-ajax-p)
      (when caption
        (htm (:h1 (cl-who:fmt caption class-name))))
      (str validation-summary)
      (loop for (key value) on fields-data  :by #'cddr
            do 
            (cl-who:htm (str value)))
      (str form-view-buttons))))

(deftemplate :template-form-view-body-wt 'template-form-view-body-wt 
             :application-class 'twitter-bootstrap-webapp)

(defun radio-view-field-wt (&key field-class content field-label show-required-indicator required-indicator-label validation-error)
  (with-html-to-string
    (:div :class (format nil "control-group ~A" field-class)
     (:label :class "control-label"
      (:span :class "slot-name"
       (:span :class "extra"
        (str field-label) ":&nbsp;"
        (when show-required-indicator
          (htm (:em :class "required-slot text-warning" (str required-indicator-label))
               (str "&nbsp;"))))))
     (:div :class (format nil "controls ~A" (when validation-error "warning"))
      (str content) 
      (when validation-error
        (htm (:p :class "validation-error"
              (:em
                (:span :class "validation-error-heading")
                (str validation-error)))))))))

(deftemplate :radio-view-field-wt 'radio-view-field-wt 
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
		     (translate (humanize-name "< Previous"))
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
		     (translate (humanize-name "Next >"))
		     :class "next-page btn")))
      ; Go to page
      (when (> (pagination-page-count obj) 1)
        (htm 
          (:div :class "pull-left"
           "&nbsp;")
          (:div :class "pull-left form-inline"
           (with-html-form (:get (curry #'pagination-on-go-to-page obj))
             (:div :class "pull-left"
              (:label (:span (str (translate "Go to page:&nbsp;")))
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
               (render-button "go-to-page" :value (translate "Go")))))))
      ; Total items
      (pagination-render-total-item-count obj)))))

