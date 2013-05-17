(in-package :ca.uoguelph.www.tag)

(defvar *base-url* "http://localhost:8080")

;; Controller

(webaction-project "tag" :project-prefix "/tag/" :index "main"
                   :map '(("home" "index.clp")))

(defun profile-add (req ent)
  (let ((form (profile-form (make-instance 'tutor-profile))))
    (if* (eq :get (request-method req))
       then (print form *html-stream*)
       else (process-form form (request-query req))
            (let ((profile (create-profile :name name :mail mail :subjects subjects))
                  (form (profile-form profile (request-query req))))
              ;; redirect to profile view
              (with-http-response (req ent :response *response-found*)
                (setf (reply-header-slot-value req :location)
                  (format nil "~a/profile/view?id=~a" *base-url*
                    (db-object-oid profile)))
                (with-http-body (req ent)))))))

(publish :path "/profile/add"
         :content-type "text/html"
         :function #'profile-add)

(defun profile-edit (req ent)
  ;; Get query values.
  (let ((id          (parse-integer (request-query-value "id" req)))
        (name        (request-query-value "name" req))
        (mail        (request-query-value "mail" req))
        (subjects    (request-query-value "subjects" req))
        (tags        (request-query-value "tags" req))
        (notes       (request-query-value "notes" req))
        (destination (request-query-value "go" req)))
    ;; Is this a get, or a post?
    (if* (eq :get (request-method req))
       then
            (let ((profile (load-profile :oid id)))
              (with-http-response (req ent)
                (with-http-body (req ent)
                  (edit-profile profile :action "/profile/edit"
                                :destination destination))))
       else
            ;; update profile
            (update-profile :oid id :name name :mail mail
                            :subjects subjects :tags tags :notes notes)
            ;; redirect to profile view
            (with-http-response (req ent :response *response-found*)
              (setf (reply-header-slot-value req :location)
                (format nil "~a~a?id=~a" *base-url* destination id))
              (with-http-body (req ent))))
    ))


(publish :path "/profile/edit"
         :content-type "text/html"
         :function #'profile-edit)


(defun profile-view (req ent)
  ;; Get query values.
  (let ((id (parse-integer (request-query-value "id" req))))
    (let ((profile (load-profile :oid id)))
      (with-http-response (req ent)
        (with-http-body (req ent)
          (profile-page profile))))))

(publish :path "/profile"
         :content-type "text/html"
         :function #'profile-view)

(defun profile-list (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (list-profiles (all-profiles)))))

(publish :path "/profile/list"
         :content-type "text/html"
         :function #'profile-list)

(defun profile-search (req ent)
  (let* ((raw-query (request-query-value "q" req))
         (query (delimited-string-to-list (or raw-query "") " "))
         (results))
    (dolist (tag query)
      (dolist (profile (tagged-profiles tag))
        (pushnew profile results)))
    (with-http-response (req ent)
      (with-http-body (req ent)
        (search-page raw-query results)))))

(publish :path "/profile/search"
         :content-type "text/html"
         :function #'profile-search)

;; API

(defun profile-api (req ent)
  ;; Get query values.
  (let ((id (parse-integer (request-query-value "id" req))))
    (if* (eq :get (request-method req))
       then
            (let ((profile (load-profile :oid id)))
              (with-http-response (req ent)
                (with-http-body (req ent)
                  (format *html-stream* "{~@{~s: ~s~^, ~}}"
                    "id" (db-object-oid profile)
                    "name" (name profile)
                    "mail" (mail profile)
                    "subjects" (subjects profile)))))
     elseif (eq :put (request-method req))
       then
            (let* ((profile (load-profile :oid id))
                   (body (get-request-body req)))
              (with-input-from-string (stream body)
                (with-http-response (req ent)
                  (with-http-body (req ent))))))))


(publish :path "/api/1/profile"
         :content-type "text/json"
         :function #'profile-api)

(defun api-v1-profile-list (req ent)
  (let ((profiles (all-profiles)))
    (with-http-response (req ent)
      (with-http-body (req ent)
        (format *html-stream* "[")
        (dolist (profile profiles)
          (format *html-stream* "{~@{~s: ~s~^, ~}}, "
            "id" (db-object-oid profile)
            "name" (name profile)
            "mail" (mail profile)
            "subjects" (subjects profile)))
        (format *html-stream* "{}]")))))

(publish :path "/api/1/profile-list"
         :content-type "text/json"
         :function #'api-v1-profile-list)

;; Static resources

(publish-directory :prefix "/css/" 
                   :destination #P"C:\\Users\\brharp\\Documents\\allegro-projects\\tag\\css")



;; Startup
(open-tutor-database "tutor.db")
(start :port 8080)
