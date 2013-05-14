;; -*- mode: common-lisp; package: data.binding -*-
;;
;; kvc.cl
;;
;; copyright (c) 2013 M. Brent Harp
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA
;;

(defpackage :data.binding
  (:use :common-lisp :excl)
  (:export #:key-value #:set-key-value))

(in-package :data.binding)



(defgeneric key-value (object key)
  (:documentation "Returns the value of key in object."))



(defgeneric set-key-value (object key value)
  (:documentation "Sets the value of key in object."))



(defsetf key-value set-key-value)



(defmethod key-value ((object t) (key string))
  "Converts string keys to symbols in the current package."
  (key-value object (intern (string-upcase key))))




(defmethod set-key-value ((object t) (key string) value)
  "Converts string keys to symbols in the current package."
  (set-key-value object (intern (string-upcase key)) value))





(defmethod key-value ((object t) (key symbol))
  "Default implementation of key-value. Returns the value of
the slot named by key."
  (slot-value object key))





(defmethod set-key-value ((object t) (key symbol) value)
  "Default implementation of set-key-value. Sets the value of
the slot named by key."
  (setf (slot-value object key) value))




