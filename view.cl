(in-package :ca.uoguelph.www.tag)


;; View

(defun navigation ()
  (html
   ((:div class "nav")
    (:ul
     (:li "Register")
     (:li "Search")
     (:li "Login")))))



(defmacro page (title content)
  `(html (:html
          (:head (:title ,title)
                 ((:link rel "stylesheet" href "/css/style.css" type "text/css")))
          (:body ((:div class :section) ,content)))))



(defun profile-page (profile)
  (page "View Profile"
        (html (:h1 (:princ-safe (name profile)))
              (:div ((:a href (mail profile)) (:princ-safe (mail profile))))
              (:div (:princ-safe (list-to-delimited-string (profile-tags profile) ", ")))
              (:div (:princ-safe (notes profile)))
              (:div ((:a :href (format nil "~a/profile/edit?id=~a&go=/profile/view" *base-url*
                                 (db-object-oid profile))) "Edit")))))

(defmethod object-html ((profile tutor-profile))
  `(:html
    (:body
     (:h1 (:princ-safe ,(name profile)))
     (:div ((:a href ,(mail profile)) (:princ-safe ,(mail profile))))
     (:div (:princ-safe ,(list-to-delimited-string (tags profile) ", ")))
     (:div (:princ-safe ,(notes profile)))
     (:div ((:a :href ,(format nil "~a/edit?oid=~a" *base-url*
                         (db-object-oid profile))) "Edit")))))


(defmethod object-edit-form ((object tutor-profile) &key (action ".") (form-method "post"))
  (make-profile-form object :action action :form-method form-method))


(defun make-profile-form (object &key (action ".") (form-method "post"))
  (make-instance 'form
    :action action
    :method form-method
    :fields (make-profile-fields object)))


(defun make-profile-fields (object)
  (list (make-instance 'input 
          :type "text"
          :name "name"
          :label "Name" 
          :value (name object)
          :on-change #'(lambda (input new-value old-value)
                         (declare (ignore input old-value))
                         (setf (name object) new-value)
                         t)
          )
        (make-instance 'input
          :type "text" 
          :name "mail"
          :label "Mail"
          :value (mail object)
          :on-change #'(lambda (input new-value old-value)
                         (declare (ignore input old-value))
                         (setf (mail object) new-value)
                         t)
          )
        (make-instance 'input
          :type "text"
          :name "tags"
          :label "Tags"
          :value (list-to-delimited-string (tags object) ", ")
          :on-change #'(lambda (input new-value old-value)
                         (let ((tags-list (delimited-string-to-list new-value ", ")))
                           (setf (tags object) tags-list)
                           t))
          )
        (make-instance 'input
          :name "notes"
          :label "Notes"
          :type "text"
          :value (notes object)
          :on-change #'(lambda (input new-value old-value)
                         (declare (ignore input old-value))
                         (setf (notes object) new-value)
                         t)
          )
        (make-instance 'fieldset
          :fields (list (make-instance 'input
                          :name "submit"
                          :type "submit"
                          :value "Submit")))
        ))




(defun edit-profile (profile &key action destination)
  (let ((id       (if profile (db-object-oid profile) ""))
        (name     (if profile (name profile) ""))
        (mail     (if profile (mail profile) ""))
        (subjects (if profile (subjects profile)))
        (tags     (if profile (profile-tags profile)))
        (notes    (if profile (notes profile) ""))
        )
    (page "Edit Profile" (print (profile-form profile) *html-stream*))
    (html (:h1 "Edit profile")
          ((:form action action :method "post")
            ;; ID
            ((:input :type "hidden" :name "id" :value id))
            ;; Destination
            ((:input :type "hidden" :name "go" :value destination :size 30))
            ;; Name
            ((:label :for "name") "Name")
            ((:input :type "text" :name "name" :value name :size 30))
            ;; Email
            ((:label :for "mail") "Email")
            ((:input :type "text" :name "mail" :value mail :size 30))
            ;; Subjects
            ((:label :for "subjects") "Subjects")
            ((:input :type "text" :name "subjects" :value subjects :size 30))
            ;; Tags
            ((:label :for "tags") "Tags")
            ((:input :type "text" :name "tags" :size 30
                     :value (list-to-delimited-string tags ", ")))
            ;; Notes
            ((:label :for "notes") "Notes")
            ((:textarea :name "notes" :rows 10)
             (:princ-safe notes))
            ;; Submit
            (:div ((:input :type "submit" :value "Submit" :class "default")))))))



(defun list-profiles (profiles)
  (html
   (:html
    (:head ((:link :rel "stylesheet" :type "text/css" :href "/css/style.css")))
    ((:body :prefix "foaf: http://xmlns.com/foaf/0.1/")
     (:ul
      (dolist (obj profiles)
        (let ((name (format nil "id~a" (db-object-oid obj)))
              (view (format nil "~a/profile/view?id=~a" *base-url* (db-object-oid obj)))
              (edit (format nil "~a/profile/edit?id=~a&go=/profile/list" *base-url* (db-object-oid obj)))
              (mail (format nil "mailto:~a" (mail obj))))
          (html ((:li :resource view :class "profile")
                 ((:h2 :typeof "foaf:Person" :class "person")
                  ((:a :name name))
                  ((:a :href view :property "foaf:name") (:princ-safe (name obj))) " "
                  ((:a :href mail :property "foaf:mbox") (:princ-safe (mail obj)))) " "
                 (:ul
                  (dolist (tag (profile-tags obj))
                    (html ((:li :property "subject") (:princ-safe tag)))))
                 ((:div :class "menu")
                  ((:a :href edit) "Edit")))))))))))

(defun search-page (&optional query results)
  (page "Search"
        (html (:h1 "Tutor Search")
              ((:form :action "/profile/search" :method "get")
               ((:input :type "text" :name "q" :value (if query query "")))
               ((:input :type "submit" :value "Search")))
              (when results
                (html (:ol (dolist (result results)
                             (html (:li ((:a :href (format nil "/profile/view?id=~a" (db-object-oid result))) 
                                         (:princ-safe (name result))))))))))))


