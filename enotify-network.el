(require 'enotify-mode-line)
(require 'enotify-messages)

(defcustom enotify-default-port 5000
  "Default port enotify uses TCP port used for client notifications"
  :group 'enotify)

(defvar enotify-port enotify-default-port)

(defcustom enotify-fallback-ports nil
  "TCP ports to try if `enotify-default-port' is busy or nil"
  :group 'enotify
  :type '(repeat integer))

(defconst enotify-process-name "Enotify")

(defcustom enotify-use-next-available-port nil
  "Whether enotify should try to bind to the next TCP port when `enotify-port' is busy.

If the value is an integer N, enotify will first try the ports
specified in `enotify-fallback-ports', and after that, being P the last port specified in `enotify-fallback-ports', it will try the ports in the interval
[P..P+enotify-use-next-available-port].
If the value is t, it will indefinitely try increasing port numbers until it finds an
abailable one."
  :group 'enotify
  :type '(choice boolean integer))

(defvar enotify-connection nil
  "Network connection/process of the enotify server")

(defun enotify-start-server-1 (port)
  "Starts the Enotify notification service"
  (setq enotify-connection (make-network-process :name enotify-process-name
						 :server t
						 :family 'ipv4
						 :service port
						 :filter 'enotify-message-filter)))

(defun enotify-start-server-2 (port)
  (condition-case err
      (progn
	(enotify-start-server-1 port)
	nil)
    (error err)))

(defun enotify-start-server-3 (port port-list try-next)
  (when (> port 65535) (error "%d is not a valid port number" port))
  (message "Trying port %d" port)
  (let ((err (enotify-start-server-2 port)))
    (if err
	(cond (port-list
	       (enotify-start-server-3 (car port-list) (cdr port-list) try-next))
	      ((and (numberp try-next) (> try-next 0))
	       (enotify-start-server-3 (1+ port) nil (1- try-next)))
	      ((or (and (numberp try-next) (zerop try-next)) (null try-next))
	       (error "[port=%d] %s" port (error-message-string err)))
	      (try-next
	       (enotify-start-server-3 (1+ port) nil try-next))
	      (t (error "[port=%d] %s" port (error-message-string err))))
      port)))
    
(defun enotify-start-server ()
  (setq enotify-port (enotify-start-server-3 enotify-port
					     enotify-fallback-ports
					     enotify-use-next-available-port)))

    
;;;###autoload
(defun enotify-port ()
  "Displays a message indicating what port is bound to the
enotify server."
  (interactive)
  (if enotify-minor-mode
      (let ((face (cond ((= enotify-port enotify-default-port)
			 :success)
			((member enotify-port enotify-fallback-ports)
			 :warning)
			(t :failure))))
	(message "Enotify running on port %s."
		 (propertize (format "%d" enotify-port)
			     'face (enotify-face face))))
    (message "Enotify not running.")))
				     
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
  (let ((mh (if (enotify/plugin:name-p message-handler)
                (enotify/plugin:handler message-handler)
              message-handler)))
    (if (functionp mh)
	(puthash id mh enotify-message-handlers-table)
      (error "Enotify: invalid slot message handler for slot-id %S" id))))


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
