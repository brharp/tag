
(in-package :ca.uoguelph.www.tag)

;; Object controller


(defun add-action (req ent)
 
  (let* ((type-name (request-query-value "type" req))
         (package-name (request-query-value "package" req))
         (type (intern type-name package-name))
         (object (make-instance type))
         (form (object-edit-form object)))
                     
    (if* (eq :get (request-method req))
       then
            (with-http-response (req ent)
              (with-http-body (req ent)
                (html 
                 (:html
                  (:head (:title "Edit")
                         ((:link rel "stylesheet" href "/css/style.css" type "text/css")))
                  (:body ((:div class :section)
                          (:princ form)))))))
       else
            (if* (submit-form form form-data)
               then 
                    (with-http-response (req ent :response *response-found*)
                      (setf (reply-header-slot-value req :location)
                        (format nil "~a/profile/view?id=~a" *base-url*
                          (db-object-oid object)))
                      (with-http-body (req ent)))
               else 
                    (with-http-response (req ent)
                      (with-http-body (req ent)
                        (print form *html-stream*)))))))


(publish :path "/add" :content-type "text/html"
         :function #'(lambda (req ent)
                       (let ((*allegrocache* *tutor-db*))
                         (add-action req ent))))



(defun edit-action (req ent)
  ;; get object by oid
  ;; bind *forms-default-action*, etc.
  ;; get form for object
  )
