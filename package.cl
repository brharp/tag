
(eval-when (compile load)
  (require :acache "acache-2.1.22.fasl")
  (require :webactions))

(defpackage :ca.uoguelph.www.tag
  (:use :common-lisp :excl :net.html.generator :net.aserve
        :db.allegrocache :net.uri :net.html.forms :data.binding)
  (:nicknames :tag))

(in-package :ca.uoguelph.www.tag)

(defparameter *tutor-db-file* "tutor.db")

(defvar *tutor-db*)

(defvar *db-lock* (mp:make-process-lock))

(defun create-tutor-database (db-file)
  (let (*allegrocache*)
    (create-file-database db-file)
    (close-database)))

(defun open-tutor-database (db-file)
  (let (*allegrocache*)
    (open-file-database db-file)
    (setf *tutor-db* *allegrocache*)))

(defun close-tutor-database ()
  (close-database :db *tutor-db*))
