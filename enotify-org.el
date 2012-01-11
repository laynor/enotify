(require 'org)

;;; Adding buffer link type to org mode
(org-add-link-type "buf" 'org-buf-open)

(add-hook 'org-store-link-functions 'org-buf-store-link)

(defun enotify-number-or-nil (string)
  (when (string-match "^\\([0-9]+\\)$" string)
    (string-to-number (match-string-no-properties 1 string))))

(defun enotify-pattern (string)
  (when (string-match "^/\\(.*\\)/$" string)
    (match-string-no-properties 1 string)))

(defun enotify-search-pattern (pattern)
  (condition-case nil
      (condition-case nil
	  (search-forward-regexp pattern)
	(error (search-backward-regexp pattern)))
    (error nil)))

(defun enotify-switch-to-buffer-at-line-or-pattern (buffer line)
  (switch-to-buffer buffer)
  (when line
    (let ((lineno (enotify-number-or-nil line))
	  (pattern (enotify-pattern line)))
      (when lineno
	(goto-line lineno))
      (when pattern
	(message "searching for pattern: %S" pattern)
	(enotify-search-pattern pattern)))))


(defcustom org-buf-command 'enotify-switch-to-buffer-at-line-or-pattern
  "The Emacs command to be used to display a buffer"
  :group 'org-link
  :type '(choice (const switch-buffer)))



(defun org-buf-open (path)
  "path should be the name of a buffer"
  (let* ((colon (position ?: path))
	 (buffername (substring path 0 colon))
	 (line (and colon (substring path (1+ colon)))))
    (message "Calling org buf command with line %S" line)
    (funcall org-buf-command buffername line)))


(defun org-buf-store-link ()
  "Store a link to a buffer."
  (unless (buffer-file-name (current-buffer))
      (let* ((page (buffer-name (current-buffer)))
	     (link (concat "buf:" page))
	     (description (format "%s buffer." page)))
	;; (message link)
	(org-store-link-props
	 :type "buf"
	 :link link
	 :description description))))

(defun enotify-buffer-name-to-raw-link (bufname &optional line)
  (concat "buf:" bufname (and line (format ":%s" line))))

(defun enotify-buffer-name-to-link (bufname descr)
  (concat "[[" (buffer-name-to-raw-link bufname) "][" descr "]]"))

(provide 'enotify-org)
