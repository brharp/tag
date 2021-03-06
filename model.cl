;; Model

(in-package :ca.uoguelph.www.tag)


;; (((((((((((( Profiles ))))))))))))

(defclass tutor-profile ()
  ((name     :initarg :name     :accessor name     :initform "")
   (mail     :initarg :mail     :accessor mail     :initform "")
   (subjects :initarg :subjects :accessor subjects :initform "")
   (notes    :initarg :notes    :accessor notes    :initform ""))
  (:metaclass persistent-class))


(defmethod key-value ((object tutor-profile) (key (eql 'tags)))
  (profile-tags object))


(defun create-profile (&key name mail subjects &allow-other-keys)
  (let ((*allegrocache* *tutor-db*))
    (prog1 (make-instance 'tutor-profile :name name
             :mail mail :subjects subjects)
      (commit))))

(defun load-profile (&key oid &allow-other-keys)
  (let ((*allegrocache* *tutor-db*))
    (oid-to-object 'tutor-profile oid)))

(defun update-profile (&key oid name mail subjects tags notes &allow-other-keys)
  (let ((*allegrocache* *tutor-db*))
    (let ((profile (oid-to-object 'tutor-profile oid)))
      (with-slots ((old-name name)
                   (old-mail mail)
                   (old-subjects subjects)
                   (old-notes notes))
          profile
        (when name     (setf old-name name))
        (when mail     (setf old-mail mail))
        (when subjects (setf old-subjects subjects))
        (when tags     (let* ((old-tags (profile-tags profile))
                              (new-tags (delimited-string-to-list tags ", "))
                              (to-remove (set-difference old-tags new-tags))
                              (to-add (set-difference new-tags old-tags)))
                         (dolist (tag to-remove) (remove-tag tag profile))
                         (dolist (tag to-add) (add-tag tag profile))))
        (when notes    (setf old-notes notes))
        )
      (commit))))

(defun delete-profile (&key oid &allow-other-keys)
  (let ((*allegrocache* *tutor-db*))
    (let ((profile (oid-to-object 'tutor-profile oid)))
      (delete-instance profile)
      (commit))))

(defun all-profiles ()
  (let ((profiles ()))
    (let ((*allegrocache* *tutor-db*))
      (doclass (obj 'tutor-profile) (push obj profiles)))
    profiles))


;; (((((((((((( Tags ))))))))))))

(defclass tag ()
  ((name   :initarg :name   :accessor tag-name   :index :any)
   (object :initarg :object :accessor tag-object :index :any))
  (:metaclass persistent-class)
  (:documentation "Tags attach text to an object. Tags are indexed on
the name field and object field, for efficient retrieval of all tags
for a given object, or all objects with a given tag."))

(defun tags (object)
  "Returns a list of tag names applied to an object."
  (mapcar #'tag-name (retrieve-from-index 'tag 'object object :all t)))

(defun (setf tags) (new-tags object)
  (let ((old-tags (tags object)))
    ;; Remove old tags.
    (dolist (tag-name (set-difference old-tags new-tags :test #'equal))
      (remove-tag tag-name object))
    ;; Add new tags.
    (dolist (tag-name (set-difference new-tags old-tags :test #'equal))
      (add-tag tag-name object)))
  new-tags)
  
(defun tagged-objects (tag-name)
  (mapcar #'tag-object (retrieve-from-index 'tag 'name name :all t)))
  
(defun add-tag (name object)
  (unless (member name (tags object) :test #'string-equal)
    (make-instance 'tag :name name :object object)))

(defun remove-tag (name object)
  (let ((tags (retrieve-from-index 'tag 'object object :all t)))
    (dolist (tag (remove name tags :key #'tag-name :test (complement #'string-equal)))
      (delete-instance tag))))
