
(defmethod set-form-data ((input input) (data string))
	"The default implementation for input controls copies
input data into the value slot."
	(setf (slot-value input 'value) data))


(defmethod print-object ((form webform) stream)
	(html-stream stream
		((:form action (action form) method (form-method form))
			(dolist (field (fields form))
				(print field stream))
			((:input type "submit")))))



(defmethod print-object ((field input) stream)
	(html-stream stream
		((:label for (name field)) (:princ (label field)))
		((:input type (type field) name (name field))
		(:if (required field) ((:span class "required") "*"))))



(defclass select (input)
 "The value of a select object is a list of values taken
 from the selected available options. Setting the raw-value of
 a select control validates that the selections are valid options."
 ((options :initform :options :accessor options)
  (value   :initform :values  :accessor values)))

(defmethod print-object ((field select) stream)
  (html-stream
    (:select
      (dolist (option (options field))
        (html ((:option value (option-value option)) (option-label option))

