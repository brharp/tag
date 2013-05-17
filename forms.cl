;; -*- mode: common-lisp; package: net.html.forms; -*-
;;
;; forms.cl
;;
;; Copyright 2013 M. Brent Harp
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
  (:export #:form #:input #:object-edit-form #:submit-form)
  (:nicknames :forms))


(in-package :net.html.forms)


(defgeneric object-edit-form (object &key action form-method)
  (:documentation "Returns a form instance for editing an object."))


(defclass form ()
  ((action :initarg :action :accessor action :type string :initform ".")
   (form-method :initarg :method :accessor form-method :type string :initform "post")
   (fields :initarg :fields :accessor fields :initform nil)
   (on-submit :initarg :on-submit :accessor on-submit))
  (:documentation "Class representing HTML forms."))


(defclass input ()
  ((name            :initarg :name            :accessor name)
   (input-type      :initarg :type            :accessor input-type)
   (value           :initarg :value           :accessor value)
   (label           :initarg :label           :accessor label)
   (on-change       :initarg :on-change       :accessor on-change))
  (:documentation "Defines an HTML input element."))


(defmethod print-object ((form form) stream)
  (let ((*html-stream* stream))
    (html ((:form :action (action form) :method (form-method form))
           (dolist (field (fields form))
             (print field stream))))))


(defmethod print-object ((input input) stream)
  (let ((*html-stream* stream))
    (html ((:label :for (name input)) (:princ (label input)))
          ((:input :name (name input) :type (input-type input) :value (value input))))))



(defmethod (setf value) :around (new-value (input input))
  (let ((old-value (value input)))
    (call-next-method new-value input)
    (let ((on-change-function (on-change input)))
      (or (null on-change-function)
          (funcall on-change-function input new-value old-value)
          (setf (value input) old-value)))))
                    


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



