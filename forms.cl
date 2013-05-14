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
  (:nicknames :forms))


(in-package :net.html.forms)



(defclass webform ()
  ((action :initarg :action :accessor action :type string)
   (form-method :initarg :method :accessor form-method :type string)
   (fields :initarg :fields :accessor fields)
   (on-submit :initarg :on-submit :accessor on-submit)
   (data-context :initarg :data-context :accessor data-context))
  (:documentation "Class representing HTML forms."))



(defmethod html-object ((form webform))
  (html ((:form action (action form) method (form-method form))
         (dolist (field (fields form))
           (html-object field)))))



(defclass input ()
  ((name       :initarg :name      :accessor name)
   (input-type :initarg :type      :accessor input-type)
   (value      :initarg :value     :accessor value)
   (label      :initarg :label     :accessor label)
   (form-data  :initarg :form-data :accessor form-data))
  (:documentation "Defines an HTML input element."))



;; Default method after setting form-data---copies form-data
;; to input value.
(defmethod (setf form-data) :after (data (input input))
  (setf (value input) data))



(defmethod html-object ((input input))
  (html ((:label for (name input)) (:princ (label input)))
        ((:input name (name input) type (input-type input)
                 value (value input)))))





    
(defun process-form (form form-data)
  "Sets values from user input, validates, and calls the submit handler."
  (dolist (field (fields form))
    (let ((data (cdr (assoc (name field) form-data :test #'equal))))
      (setf (form-data field) data))))
                    
	




(let ((test-form (make-instance 'webform
                   :action "action" :method "post"
                   :fields (list (make-instance 'input :name "foo" :type "text" :value "" :label "Foo")))))
  (process-form test-form '(("foo" . "Hello, World!")))
  (let ((*html-stream* *standard-output*))
    (html-object test-form)))
