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
  (puthash connection (get-buffer-create (format " %S" connection))
	   enotify-mp-cmbt))

(defun enotify-mp-reinit ()
  (clrhash enotify-mp-cmbt))
;; get the buffer associated to a client connection
(defun enotify-mp-buffer (connection)
  (or (gethash connection enotify-mp-cmbt)
      (enotify-mp-allocate-buffer connection)))


;; Store DATA in the buffer associated to CONNECTION
(defun enotify-mp-store-data (connection data)
  (let ((buffer (enotify-mp-buffer connection)))
    (save-current-buffer 
      (set-buffer buffer)
      (goto-char (point-max))
      (insert data))))

;; Get a message from the connection (if present)
(defvar enotify-mp-size-regex "|\\([[:digit:]]+\\)|")

(defun enotify-mp-get-message (connection)
  (let ((buf (if (bufferp connection) connection (enotify-mp-buffer connection))))
    (save-current-buffer
      (set-buffer buf)
      (goto-char (point-min))
      (let ((msg-start (re-search-forward enotify-mp-size-regex nil t)))
	(when msg-start
	  (let ((header-len (length (match-string 0)))
		(len (string-to-int (match-string 1))))
	    (delete-region 1 (- msg-start header-len))
	    (when (>= (- (point-max) header-len) len)
	      (let ((msg (buffer-substring (1+ header-len) (+ header-len 1 len))))
		(delete-region 1 (min (point-max) (+ header-len 1 len)))
		msg))))))))

(defun enotify-mp-lscb ()
  (maphash (lambda (k v)
	     (princ (buffer-name v)))
	   enotify-mp-cmbt)
  (princ "\n")
  nil)
      
(provide 'enotify-messages)