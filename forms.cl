;; -*- mode: common-lisp; package: net.html.forms
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

(require :aserve)

(defpackage :net.html.forms
  (:use :common-lisp :excl :net.html.generator)
  (:nicknames :forms))

(in-package :net.html.forms)

(defclass webform ()
  ((action :initarg :action :accessor action :type string)
   (method :initarg :method :accessor form-method :type string)
   (fields :initarg :fields :accessor fields))
  (:documentation "Class representing HTML forms."))



(defmethod print-object ((form webform) stream)
  "Prints an HTML form."
  (let ((*html-stream* stream))
    (html ((:form action (action form) method (form-method form))
           (do-list (field (fields form))
             (print field stream))))))



(defclass input ()
  ((name  :initarg :name  :accessor name)
   (type  :initarg :type  :accessor input-type)
   (value :initarg :value :accessor value)
   (label :initarg :label :accessor label))
  (:documentation "Defines an HTML input element."))




(defmethod print-object ((input input) stream)
  (let ((*html-stream* stream))
    (html ((:label for (name input)) (:princ (label input)))
          ((:input name (name input) type (input-type input)
                   value (value input))))))



(defgeneric form-data (input)
  (:documentation "Returns form data for a control (as a string.)"))


(defgeneric set-form-data (input data)
  (:documentation "Sets form data for a control."))


(defsetf form-data set-form-data)


(defmethod form-data ((input input))
  "The default implementation of form-data. Returns the value slot."
  (slot-value input 'value))



(defmethod set-form-data ((input input) (data string))
  "The default implementation of set-form-data."
  (setf (slot-value input 'value) data))





(defun process-form (form form-data)
  "Sets the form-data of each control.")






