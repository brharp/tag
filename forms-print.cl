
(defmethod print-object ((form webform) stream)
	(html-stream stream
		((:form action (action form) method (form-method form))
			(dolist (field (fields form))
				(print field stream)))))
