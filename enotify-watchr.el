;;;; Message handler for rspec test results

(defun enotify-rspec-result-buffer-name (id)
  (format "*RSpec Results: %s*" id))

(defun enotify-rspec-result-message-handler (id data)
  (let ((buf (get-buffer-create (enotify-rspec-result-buffer-name id))))
    (save-current-buffer
      (set-buffer buf)
      (erase-buffer)
      (insert data)
      (flet ((message (&rest args) (apply 'format args)))
	(org-mode)))))
      


(defvar enotify-rspec-result-message-handler 'enotify-rspec-result-message-handler)

(defun enotify-rspec-mouse-1-handler (event)
  (interactive "e")
  (switch-to-buffer-other-window
   (enotify-rspec-result-buffer-name
    (enotify-event->slot-id event))))

(defvar enotify-rspec-mouse-1-handler 'enotify-rspec-mouse-1-handler)

(provide 'enotify-watchr)
