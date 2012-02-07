(require 'enotify-group)
(require 'enotify-mode-line)
(require 'enotify-network)

(defvar enotify-idle-timer nil)
;;;###autoload
(define-minor-mode enotify-minor-mode
  "Toggle display of notifications in the mode line."
  :global t :group 'enotify
  (setq enotify-mode-line-string nil)
  (or global-mode-string (setq global-mode-string (list "")))
  (cond ((not enotify-minor-mode)
	 (setq global-mode-string
	       (delq 'enotify-mode-line-string global-mode-string))
	 (when (timerp enotify-idle-timer)
	   (cancel-timer enotify-idle-timer)
	   (setq enotify-idle-timer nil))
	 (delete-process enotify-connection))
	(t (add-to-list 'global-mode-string 'enotify-mode-line-string t)
	   (enotify-init-network)
	   (setq enotify-idle-timer
		 (run-with-timer enotify-mp-idle-time enotify-mp-idle-time
				 'enotify-mp-clean-garbage-timer))
	   (enotify-mode-line-update))))

;;;###autoload
(defun enotify-version ()
  (interactive)
  (message "VERSION"))

;;;###autoload
(defun enotify-README ()
  (interactive)
  (browse-url "https://github.com/laynor/enotify/blob/master/README.md"))


(provide 'enotify)
