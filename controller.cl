
(in-package :ca.uoguelph.www.tag)

;; Object controller

(defun add-action (req ent)
  
  (let* ((type-name (request-query-value "type" req))
         (type      (intern type-name))
         (object    (make-instance type))
         (form      (object-webform object)))
    
    (flet ((show-form ()
              (with-http-header (req ent)
                (with-http-body (req ent)
                  (print form *html-stream*)))))
      
      (if* (eq :get (request-method req))
         then
              (show-form)
              (rollback)
         else
              (if* (submit-form form form-data)
                 then (redirect-to object req ent)
                 else (show-form))
              ))))


(publish :path "/add" :content-type "text/html" :function #'add-action)

(defun edit-action (req ent)
  ;; get object by oid
  ;; bind *forms-default-action*, etc.
  ;; get form for object
  )
