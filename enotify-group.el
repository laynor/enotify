(defgroup enotify nil
  "Display notifications on emacs' mode line."
  :group 'modeline)
(eval-and-compile (provide 'enotify-group))

(defvar enotify-minor-mode) ;; Silencing compiler

(defcustom enotify-debug nil
  "Enable debug messages."
  :group 'enotify)
