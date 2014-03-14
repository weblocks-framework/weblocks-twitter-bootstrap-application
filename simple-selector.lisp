(in-package :weblocks-twitter-bootstrap-application)

(defwidget simple-selector ()
  ((selected :initarg :selected :initform nil :accessor simple-selector-selected)
   (children :initarg :children :initform nil)
   (child-callbacks :initarg :child-callbacks)
   (names :initarg :names)))

(defmethod initialize-instance :after ((obj simple-selector) &rest args)
  (update-chooser-children obj))

(defmethod update-chooser-children ((obj simple-selector))
  (with-slots (selected children names child-callbacks) obj
    (setf (slot-value obj 'children) 
          (loop for callback in child-callbacks
                for title in names
                for i from 0 to (1- (length names))
                collect
                (cons 
                  title
                  (make-instance 
                    'composite 
                    :widgets (list (funcall callback))))))))

(defmethod render-widget-body ((widget simple-selector) &rest args)
  (declare (ignore widget args)))

(defmethod get-selected-widget ((widget simple-selector))
  (with-slots (selected) widget
    (let ((child-to-render 
            (loop for i in (slot-value widget 'children)
                  for j in (slot-value widget 'names)
                  if (or (not selected)
                         (string= selected j))
                  return i)))
      (cdr child-to-render))))

(defmethod render-widget-children ((widget simple-selector) &rest args)
  (render-widget (get-selected-widget widget)))

(defmethod widget-children ((obj simple-selector) &optional type)
  (list (get-selected-widget obj)))

(defun make-selector (name &rest args)
  (let ((names  (mapcar #'third args)))
    (setf (car names) "")
    (make-instance 'simple-selector 
                 :names names
                 :child-callbacks (mapcar (lambda (item) (lambda () item)) (mapcar #'second args))
                 :uri-id (alexandria:make-keyword (string-upcase name))
                 :public-parameters '(:selected))))
