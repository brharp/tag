(in-package :net.aserve)

(defclass locator-regex (locator)
  ()
  )

(defstruct (regex-handler (:type list))
  pattern     ;; pattern which url must match
  host-handlers
  )

(defun publish-regex (&key (host nil host-p) port pattern
			    function class format
			    content-type
			    (server *wserver*)
			    locator
			    remove
			    authorizer
			    timeout
			    plist
			    headers
			    (compress t)
			    )
  ;; publish a handler for all urls that match a 
  ;; certain regular expression
  ;; 
  (let (hval)
    (if* (null locator) 
       then (setq locator (find-locator :regex server)))

    (setq hval (convert-to-vhosts (if* (and host (atom host))
				     then (list host)
				     else host)
				  server))
    
    (if* remove
       then ; eliminate the entity if it exists
	    (publish-regex-entity nil pattern locator hval host-p t)
	    nil
       else
	     
	    (let ((ent (make-instance (or class 'computed-entity)
			 :host hval 
			 :port port
			 :pattern pattern
			 :function function
			 :format format
			 :content-type content-type
			 :authorizer authorizer
			 :plist plist
			 :timeout timeout
			 :headers headers
			 :compress compress
			 )))
	      (publish-regex-entity ent pattern locator  hval
				     host-p nil)
	      ent))))

(defun publish-regex-entity (ent pattern locator host host-p remove)
  ;; add or remove an entity ent from the locator
  ;;
  (dolist (entpair (locator-info locator))
    (if* (equal (regex-handler-path entpair) pattern)
       then ; match, pattern
	    (if* (and remove (not host-p))
	       then ; remove all entries for all hosts
		    (setf (locator-info locator)
		      (remove entpair (locator-info locator)))
		    (return-from publish-regex-entity nil))
	    

	    (let ((handlers (regex-handler-host-handlers entpair)))
	      (dolist (host host)
		(dolist (hostpair handlers
			  ; not found, add it if we're not removing
			  (if* (not remove)
			     then (push (make-host-handler :host host
							   :entity ent)
					handlers)))
		  (if* (eq host (host-handler-host hostpair))
		     then ; a match
			  (if* remove
			     then (setq handlers
				    (remove hostpair handlers :test #'eq))
			     else ; change
				  (setf (host-handler-entity hostpair) ent))
			  (return))))
	      (setf (regex-handler-host-handlers entpair) handlers))
	    
	    ; has been processed, time to leave
	    (return-from publish-regex-entity ent)))

  ; pattern not present, must add.
  
  (if* remove 
     then ; no work to do
	  (return-from publish-regex-entity nil))
  
  (let ((list (locator-info locator))
	(new-ent (make-regex-handler
		  :pattern pattern
		  :host-handlers (mapcar #'(lambda (host)
					     (make-host-handler 
					      :host host 
					      :entity ent))
					 host))))
    (if* (null list)
       then ; this is the first
	    (setf (locator-info locator) (list new-ent))
       else ; add to front of list
            (setf (locator-info locator) (cons new-ent list)))))

(defmethod standard-locator ((req http-request)
			     (locator locator-regex))
  ;; standard function for finding an entity in a regex locator
  ;; return the entity if one is found, else return nil
  
  (if* (uri-scheme (request-raw-uri req))
     then ; ignore proxy requests
	  (return-from standard-locator nil))
  
  (let* ((url (request-decoded-uri-path req))
	 (len-url (length url))
	 (vhost (request-vhost req)))
	     
    (dolist (entpair (locator-info locator))
      (if* (match-re (regex-handler-pattern entpair) url)
	 then
              (let ((hh (or (assoc vhost (regex-handler-host-handlers
                                          entpair)
                                   :test #'eq)
                            (assoc :wild (regex-handler-host-handlers
                                          entpair)
                                   :test #'eq))))
                (if* hh
                   then (return (host-handler-entity hh))))))))

