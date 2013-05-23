
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
                 (html (:html (:head (:title "Add")
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
         else
              (if* (submit-form form form-data)
                 then (send-redirect)
                 else (show-form))))))






(publish :path "/add" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (add-action req ent)
                             (rollback))
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
                             (view-action req ent)
                             (rollback))
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
                             (list-action req ent)
                             (rollback))
                         (condition (c)
                           (with-http-response (req ent)
                             (with-http-body (req ent)
                               (html (:princ-safe c))))))))

           






  
(defun edit-action (req ent)
  
  "Edit a persistent database object."
  
  (let* ((oid (parse-integer (request-query-value "oid" req)))
         (object (oid-to-object* t oid))
         (action (format nil "/edit?oid=~a" oid))
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
         else
              (if* (submit-form form form-data)
                 then (send-redirect)
                 else (show-form))))))


(publish :path "/edit" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (edit-action req ent)
                             (rollback))
                         (condition (c)
                                    (with-http-response (req ent)
                                      (with-http-body (req ent)
                                        (html (:princ-safe c))))))))




(defun delete-action (req ent)
  
  "Delete a persistent database object."
  
  (let* ((oid (parse-integer (request-query-value "oid" req)))
         (type-name (request-query-value "type" req))
         (package-name (request-query-value "package" req))
         (type (intern type-name package-name))
         (object (oid-to-object type oid)))
    
    (delete-instance object)
    (commit)
    
    (with-http-response (req ent :response *response-found*)
      (setf (reply-header-slot-value req :location)
        (format nil "~a/list?type=~a&package=~a" *base-url* type-name package-name))
      (with-http-body (req ent)))))






(publish :path "/delete" :content-type "text/html"
         :function #'(lambda (req ent)
                       (handler-case
                           (let ((*allegrocache* *tutor-db*))
                             (delete-action req ent)
                             (rollback))
                         (condition (c)
                           (with-http-response (req ent)
                             (with-http-body (req ent)
                               (html (:princ-safe c))))))))


(publish-prefix 
 :path "/profile"
 :function #'(lambda (req ent)
               (re-case (request-path (request-uri req))
                        ("profile/.*" () (list it))
                        ("foo the (.*)" ((it 1)) (list it))
                        (t :no-match))))


(defun get-profile (req ent)
  (let* ((args (rest (uri-parsed-path (request-uri req))))
         (profile-id (parse-integer (second args)))
         (profile (oid-to-object 'tutor-profile profile-id)))
    (with-http-response (req ent)
      (with-http-body (req ent)
        (html
         (:html
          (:head)
          (:body
           (:h1 (:princ (profile-name profile))))))))))

         
    
