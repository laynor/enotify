;;;;
;;;; Enotify Message passing
;;;;

;;; Message Buffer Table
;;; Each opened connection has its own message buffer.
;;; Associations between client connections and buffers
;;; are stored in a hash

;; enotify connection --> message buffer table
(defvar enotify-mp-cmbt (make-hash-table :test 'equal))

;; allocate a buffer for a connection
(defun enotify-mp-allocate-buffer (connection)
  (puthash connection (get-buffer-create (format " Enotify-msg-buffer:%S" connection))
	   enotify-mp-cmbt))

(defvar enotify-mp-idle-time 60
  "Idle time before calling enotify-mp-clean-garbage (internal
message buffer cleaning for dead connections")

(defun enotify-mp-reinit ()
  (clrhash enotify-mp-cmbt))

;;; get the buffer associated to a client connection
(defun enotify-mp-buffer (connection)
  (or (gethash connection enotify-mp-cmbt)
      (enotify-mp-allocate-buffer connection)))

;;; Store DATA in the buffer associated to CONNECTION
(defun enotify-mp-store-data (connection data)
  (let ((buffer (enotify-mp-buffer connection)))
    (save-current-buffer 
      (set-buffer buffer)
      (goto-char (point-max))
      (insert data))))

;;; Get a message from the connection (if present)
;; regex matching message size
(defvar enotify-mp-size-regex "|\\([[:digit:]]+\\)|")
(defun enotify-mp-get-message (connection)
  (let ((buf (if (bufferp connection) connection (enotify-mp-buffer connection))))
    (save-current-buffer
      (set-buffer buf)
      (goto-char (point-min))
      (let ((msg-start (re-search-forward enotify-mp-size-regex nil t)))
	(when msg-start
	  (let ((header-len (length (match-string 0)))
		(len (string-to-number (match-string 1))))
	    (delete-region 1 (- msg-start header-len))
	    (when (>= (- (point-max) header-len) len)
	      (let ((msg (buffer-substring (1+ header-len) (+ header-len 1 len))))
		(delete-region 1 (min (point-max) (+ header-len 1 len)))
		msg))))))))

;;; Utility function to print the internal buffers used for message
;;; passing
(defun enotify-mp-lscb ()
  (maphash (lambda (k v)
	     (print (buffer-name v)))
	   enotify-mp-cmbt)
  nil)
(defun enotify-mp-lsib ()
  (delq nil
   (mapcar (lambda (b)
	     (let ((bname (buffer-name b))
		   (pattern " Enotify-msg-buffer:"))
	       (when (and (>= (length bname) (length pattern))
			  (string= (substring bname 0 (length pattern))
				   pattern))
		 bname)))
	   (buffer-list))))
		
(defvar enotify-mp-cgrbg-count 0)
;;; Dead connection cleaning
(defun enotify-mp-clean-garbage ()
  "Calls delete-process for all the dead connections to the
Enotify server and kills all the related buffers."
  (when enotify-debug
    (message "Calling enotify-mp-clean-garbage %S" enotify-mp-cgrbg-count)
    (setq enotify-mp-cgrbg-count (1+ enotify-mp-cgrbg-count)))
  (let (dead-connections)
    (maphash (lambda (conn buff)
	       (when (memq (process-status conn) '(closed failed))
		 (kill-buffer buff)
		 (delete-process conn)
		 (push conn dead-connections)))
	     enotify-mp-cmbt)
    (dolist (dc dead-connections)
      (remhash dc enotify-mp-cmbt))))

(eval-and-compile (provide 'enotify-messages))
