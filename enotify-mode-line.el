;;;; Enotify mode

(defvar enotify-mode-line-string nil
  "String to display in the mode line.")

(put 'enotify-mode-line-string 'risky-local-variable t)

(defcustom enotify-mode-line-prefix "[ "
  "Text to display before the notification text in the mode-line."
  :type 'string
  :group 'enotify)

(defcustom enotify-mode-line-suffix " ]"
  "Text to display after the notification text in the mode-line."
  :type 'string
  :group 'enotify)

(defconst enotify-success-face 'enotify-success-face
  "face to fontify Enotify Success messages")

(defface enotify-success-face
  '((((class color))
     (:foreground "#228833" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Success messages"
  :group 'enotify)

(defconst enotify-failure-face 'enotify-failure-face
  "face to fontify Enotify Failure messages")

(defface enotify-failure-face
  '((((class color))
     (:foreground "red" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Failure messages"
  :group 'enotify)

(defconst enotify-warning-face 'enotify-warning-face
  "face to fontify Enotify Failure messages")

(defface enotify-warning-face
  '((((class color))
     (:foreground "goldenrod4" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Warning messages"
  :group 'enotify)

(defconst enotify-normal-face nil
  "face to notify Enotify Standard messages")

(defvar enotify-faces-alist (copy-sequence `((:standard . nil)
					     (:success . ,enotify-success-face)
					     (:warning . ,enotify-warning-face)
					     (:failure . ,enotify-failure-face))))

(defun enotify-face (face)
  (or (if (assoc face enotify-faces-alist)
	  (cdr (assoc face enotify-faces-alist))
	face)
      enotify-normal-face))

;;; Notification format:
;;; (:text <message>
;;;  :face :warning|:standard|:failure|:success|face
;;;  :mouse-1 <click-handler>
;;;  :help <tooltip text>)
(defvar enotify-mode-line-notifications-table (make-hash-table :test 'equal)
  "Contains the associations between notification \"icons\" and slot
ids.")

(defun enotify-mode-line-notification (slot-id)
  (gethash slot-id enotify-mode-line-notifications-table))

(defvar enotify-mode-line-notifications-separator
  (propertize " | " 'face enotify-normal-face))

(defun enotify-event->text (event)
  "Returns the text object associated to the mouse click event EVENT."
  (car (nth 4 (nth 1 event))))

(defun enotify-icon->slot-id (icon-text)
  "Returns the slot id for ICON-TEXT."
  (get-text-property 0 'slot-id icon-text))

(defun enotify-event->slot-id (mouse-click-event)
  "Returns the slot id of the icon clicked."
  (enotify-icon->slot-id (enotify-event->text mouse-click-event)))

(defun enotify-list-inject (list separator)
  "Returns a new list whose elements are the same of LIST but
interlaced with SEPARATOR."
  (let ((res))
    (mapc (lambda (el)
	    (push separator res)
	    (push el res))
	  list)
    (cdr (reverse res))))


(defun enotify-delete-slot-handler (event)
  "Enotify Mouse event handler that removes a notification icon."
  (interactive "e")
  (enotify-mode-line-remove-notification (enotify-event->slot-id event)))

(defun enotify-delete-slot (slot-id)
  (enotify-mode-line-remove-notification slot-id))

(defvar enotify-popup-menu
  (easy-menu-create-menu "Enotify" '(["Delete Slot" (lambda (event) (interactive "e")
						      (enotify-delete-slot (enotify-event->slot-id event)))]))
  "Menu keymap for the predefined enotify popup menu.")

(defun enotify-change-notification-face (slot-id new-face)
  (destructuring-bind (&key text face mouse-1 help)
      (enotify-mode-line-notification slot-id)
    (enotify-mode-line-update-notification slot-id (list :text text :face new-face :mouse-1 mouse-1 :help help))))

;; TODO: aggiungere handler per il bottone destro del mouse
(defun enotify-propertize-notification (slot-id notification)
  "Returns a properly propertized text object given SLOT-ID and
  NOTIFICATION.
NOTIFICATION has to be specified in this format:
  (:text <message>
   :face :warning|:standard|:failure|:success|face
   :mouse-1 <click-handler>
   :help <tooltip text>)
the tooltip text should also contain the help text for mouse-1.
The mouse-1 handler should be an (interactive \"e\") command. The
slot-id of the icon clicked can be retrieved using
`enotify-event->slot-id'."
  (destructuring-bind (&key text face mouse-1 help)
      notification
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] mouse-1)
      (define-key map [mode-line mouse-2] 'enotify-delete-slot-handler)
      (define-key map [mode-line C-mouse-1] (lambda (event)
					      (interactive "e")
					      (popup-menu enotify-popup-menu event event))) 
      (propertize text
		  'face (enotify-face face)
		  'help-echo (concat (format "[ %s ]\n" slot-id) help
				     "\nmouse-2: remove notification icon.\nC-mouse-1: Enotify popup menu")
		  'mouse-face (enotify-face face)
		  'slot-id slot-id
		  'local-map map))))
  
(defun enotify-mode-line-notifications-list ()
  "Returns a list with the notifications properly sorted and `propertize'd."
  (let (res)
    (maphash (lambda (slot-id notification)
	       (push (enotify-propertize-notification slot-id notification)
		     res))
	     enotify-mode-line-notifications-table)
    (reverse res)))

(defun enotify-mode-line-update ()
  "Updates the Enotify notification area."
  (interactive)
  (let ((res nil))
    (setq enotify-mode-line-string
	  (append 
	   (list enotify-mode-line-prefix)
	   (enotify-list-inject (enotify-mode-line-notifications-list)
				enotify-mode-line-notifications-separator)
	   (list enotify-mode-line-suffix)))))

(defun enotify-mode-line-update-notification (slot-id notification &optional pos)
  "Updates the notification \"icon\" associated with SLOT-ID to
NOTIFICATION.
NOTIFICATION has to be specified in this format:
  (:text <message>
   :face :warning|:standard|:failure|:success|face
   :mouse-1 <click-handler>
   :help <tooltip text>)"
  (puthash slot-id notification enotify-mode-line-notifications-table)
  (enotify-mode-line-update))

(defalias 'enotify-notify 'enotify-mode-line-update-notification)

(defun enotify-mode-line-remove-notification (slot-id)
  "Removes the notification \"icon\" associated with SLOT-ID from the notification area."
  (remhash slot-id enotify-mode-line-notifications-table)
  (enotify-mode-line-update)
  (force-mode-line-update))


(eval-and-compile (provide 'enotify-mode-line))

;;; enotify-mode-line.el ends here
