
(in-package :ca.uoguelph.www.tag)

;; Object controller

(defun add-action (req ent)
  
  "Adds a new persistent object to the database."
  
  (let* ((type-name (request-query-value "type" req))
         (package-name (request-query-value "package" req))
         (type (intern type-name package-name))
         (object (make-instance type))
         (action (format nil "/add?type=~a&package=~a" type-name package-name))
         (form (object-edit-form object :action action))
         (form-data (request-query req)))
    
    (flet ((show-form ()
             (with-http-response (req ent)
               (with-http-body (req ent)
                 (html (:html (:head (:title "Edit")
                                     ((:link rel "stylesheet" href "/css/style.css" type "text/css")))
                              (:body ((:div class :section)
                                      (:princ form))))))))
           
           (send-redirect ()
              (with-http-response (req ent :response *response-found*)
                (setf (reply-header-slot-value req :location)
                  (format nil "~a/view?oid=~a" *base-url* (db-object-oid object)))
                (with-http-body (req ent)))))
                      
      (if* (eq :get (request-method req))
         then
              (show-form)
              (rollback)
         else
              (if* (submit-form form form-data)
                 then (send-redirect)
                 else (show-form))))))


(publish :path "/add" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (add-action req ent)
                             (commit))
                         (condition (c)
                                    (with-http-response (req ent)
                                      (with-http-body (req ent)
                                        (html (:princ-safe c))))))))


(defun view-action (req ent)
  
  "Displays an object from the database."
  
  (let* ((oid (parse-integer (request-query-value "oid" req)))
         (object (oid-to-object 'tutor-profile oid)))
    (with-http-response (req ent)
      (with-http-body (req ent)
        (html-print (object-html object) *html-stream*)))))
    


(publish :path "/view" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (view-action req ent))
                         (condition (c)
                                    (with-http-response (req ent)
                                      (with-http-body (req ent)
                                        (html (:princ-safe c))))))))



(defun list-action (req ent)
  
  "List database objects."
  
  (let* ((type-name (request-query-value "type" req))
         (package-name (request-query-value "package" req))
         (type (intern type-name package-name))
         (objects ()))
    
    (doclass (obj 'tutor-profile)
             (push (object-html obj) objects))
    
    (with-http-response (req ent)
      (with-http-body (req ent)
        (html-print-list objects *html-stream*)))))



(publish :path "/list" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (list-action req ent))
                         (condition (c)
                                    (with-http-response (req ent)
                                      (with-http-body (req ent)
                                        (html (:princ-safe c))))))))

           

  
(defun edit-action (req ent)
  ;; get object by oid
  ;; bind *forms-default-action*, etc.
  ;; get form for object
  )
