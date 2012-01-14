(require 'enotify-group)
(require 'enotify-mode-line)
(require 'enotify-network)

;;;###autoload
(define-minor-mode enotify-minor-mode
  "Toggle display of notifications in the mode line."
  :global t :group 'enotify
  (setq enotify-mode-line-string nil)
  (or global-mode-string (setq global-mode-string (list "")))
  (if (not enotify-minor-mode)
      (setq global-mode-string
	    (delq 'enotify-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'enotify-mode-line-string t)
    (enotify-init-network)
    (enotify-mode-line-update)))

(provide 'enotify)
