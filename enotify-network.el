(require 'enotify-mode-line)
(require 'enotify-messages)

(defcustom enotify-port 5000
  "TCP port used for client notifications"
  :group 'enotify)

(defconst enotify-process-name "Enotify")

(defvar enotify-connection nil
  "Network connection/process of the enotify server")

(defun enotify-start-server ()
  "Starts the Enotify notification service"
  (setq enotify-connection (make-network-process :name enotify-process-name
						 :server t
						 :family 'ipv4
						 :service enotify-port
						 :filter 'enotify-message-filter)))

;;; Notification slot registration
;;; Slot identification:
;;; - named slot
;;; - client ip+port
;;; Filter function alist:
;;;  Each cons cell is composed by:
;;;  (identification . filter-function)
;;;  identification can be:
;;;  slot-name
;;;  ip-address:port


(defvar enotify-message-handlers-table (make-hash-table :test 'equal))

(defun enotify-register-network-slot (id message-handler)
  "Registers a slot identified by ID, handling the messages with MESSAGE-HANDLER"
    (if (functionp message-handler)
	(puthash id message-handler enotify-message-handlers-table)
      (error "Enotify: invalid slot message handler for slot-id %S" id)))


(defun enotify-hash-has-key? (key table)
  (or (gethash key table)
      (not (gethash key table t))))

(defun enotify-slot-registered? (slot)
  (enotify-hash-has-key? slot enotify-message-handlers-table))

(defun enotify-connection-id (network-connection)
  "Returns the slot id for NETWORK-CONNECTION.
This id can be used if the message has :id :connection"
  (let ((string (format "%S" network-connection)))
    (string-match "^#<.*<\\(.*\\)>>$"
		  string)
    (match-string 1 string)))

(defun enotify-message-handler (slot-id)
  "Returns the message handler associated with SLOT-ID."
  (gethash slot-id enotify-message-handlers-table))

(defun enotify-slot-id (network-connection message-id)
  "Returns the SLOT-ID that matches CONNECTION and MESSAGE-ID"
  (if (eql message-id :connection)
      (enotify-connection-id network-connection)
    message-id))


(defun enotify-message-filter (network-connection msg)
  "Dispatches the incoming message MSG from NETWORK-CONNECTION to the
right message handler."
  (enotify-mp-store-data network-connection msg)
  (let ((msg-data (enotify-mp-get-message network-connection)))
    (when msg-data
      (let ((message (car (read-from-string msg-data))))
	(condition-case err 
	    (destructuring-bind (&key id notification data register handler-fn)
		message
	      (cond ((not (null register)) ; Registration message
		     (enotify-register-network-slot
		      register
		      (or handler-fn
			  (lambda (id data)
			    (message "Ignored data: %S" data)))))

		    ((and id notification data) ; Notification message
		     (let* ((message-handler (enotify-message-handler
					      (enotify-slot-id network-connection id))))
		       (cond (message-handler  ; Slot or connection registered
			      (enotify-mode-line-update-notification id notification)
			      (funcall message-handler id data))
			     ;; Unregistered clients
			     (t (message "Enotify: Unregistered client %S: %S"
					 (enotify-connection-id network-connection)
					 message)))))
		    ;; Invalid message format
		    (t (error "some arguments were not specified correctly"))))
	  ;; Error in message format
	  (error (error "Enotify: Bad message from <%s>:: %S -> %S"
			(enotify-connection-id network-connection)
			message err)))))))
	    
(defun enotify-unregister-network-slot (slot-id)
  "Unregisters the slot identified by SLOT-ID."
  (remhash slot-id enotify-message-handlers-table))

;;; FIXME: close connections?
(defun enotify-init-network ()
  (enotify-start-server)
  (clrhash enotify-message-handlers-table))

(eval-and-compile (provide 'enotify-network))
