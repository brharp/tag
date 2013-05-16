;; -*- mode: common-lisp; package: net.html.forms; -*-
;;
;; forms.cl
;;
;; Copyright 2013 M Brent Harp
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(eval-when (:load :compile)
  (require :aserve))


(defpackage :net.html.forms
  (:use :common-lisp :excl :net.html.generator)
  (:export #:webform #:input #:object-webform
           #:object-html #:submit-form #:*request-url*)
  (:nicknames :forms))


(in-package :net.html.forms)


(defvar *request-url*)


(defgeneric object-webform (object)
  (:documentation "Returns a webform instance for editing object."))


(defclass webform ()
  ((action :initarg :action :accessor action :type string :initform ".")
   (form-method :initarg :method :accessor form-method :type string :initform "post")
   (fields :initarg :fields :accessor fields :initform nil)
   (on-submit :initarg :on-submit :accessor on-submit)
   (data-context :initarg :data-context :accessor data-context))
  (:documentation "Class representing HTML forms."))


(defmethod object-html ((form webform))
  `((:form :action ,(action form) :method ,(form-method form))
    ,@(mapcar #'object-html (fields form))))



(defclass input ()
  ((name            :initarg :name            :accessor name)
   (input-type      :initarg :type            :accessor input-type)
   (value           :initarg :value           :accessor value)
   (label           :initarg :label           :accessor label)
   (form-data       :initarg :form-data       :accessor form-data)
   (on-value-change :initarg :on-value-change :accessor on-value-change))
  (:documentation "Defines an HTML input element."))

(defmethod object-html ((input input))
  `(((:label :for ,(name input)) (:princ ,(label input)))
    ((:input :name ,(name input) :type ,(input-type input) :value ,(value input)))))

(defmethod (setf value) :after (new-value (input input))
  (let ((on-value-change))
    (when (setq on-value-change (on-value-change input))
      (funcall on-value-change new-value))))






    
(defun process-form (form form-data)
  "Sets values from user input, validates, and calls the submit handler."
  (dolist (field (fields form))
    (let ((data (cdr (assoc (name field) form-data :test #'equal))))
      (setf (form-data field) data))))
                    


(defun submit-form (form form-data)
  ;; Iterate over form inputs.
  (do* ((form-data   form-data        (cdr form-data))
        (input       (car form-data)  (car form-data))
        (input-name  (car input)      (car input))
        (input-value (cdr input)      (cdr input)))
       ((null form-data) t)
    ;; Lookup the form field for each input, and set input-value.
    (let ((field (find input-name (fields form) :key #'name :test #'equal)))
      (when field (setf (value field) input-value)))))





(let ((test-form (make-instance 'webform
                   :action "action" :method "post"
                   :fields (list (make-instance 'input :name "foo" :type "text" :value "" :label "Foo")))))
  (process-form test-form '(("foo" . "Hello, World!")))
  (let ((*html-stream* *standard-output*))
    (html-object test-form)))
