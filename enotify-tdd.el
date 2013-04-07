;;;; Espectator plugin for enotify
(require 'enotify)

(defgroup enotify/tdd nil
  "TDD (red/green) plugin for Enotify")

(defun enotify/tdd:test-result-buffer-name (slot-id)
  (format "*%s test results*" slot-id))

(defun enotify/tdd::major-mode-fn (mode-name)
  (concat mode-name "-mode"))

(make-face 'enotify/tdd::normal-face)

(defvar enotify/tdd::blink-table (make-hash-table :test 'equal))

(defun enotify/tdd::blink-helper (slot-id)
  (cond ((null (enotify-mode-line-notification slot-id))
         ;; cancel the timer if the notification has been closed
         (cancel-timer (car (gethash slot-id enotify/tdd::blink-table))))
        (t (destructuring-bind (timer original-face current-face)
               (gethash slot-id enotify/tdd::blink-table)
             (let ((face (if (eq original-face current-face)
                             'enotify/tdd::normal-face
                           original-face)))
               (puthash slot-id (list timer original-face face) enotify/tdd::blink-table)
               (enotify-change-notification-face slot-id face)
               (redraw-modeline))))))

(defcustom enotify/tdd:blink-delay nil
  "The notification blinking delay in seconds. NIL or 0 mean no blinking."
  :group 'enotify/tdd
  :type 'number)

(defcustom enotify/tdd:enable-message-notification t
  "Whether or not to display a message containing the test
  summary in the minibuffer"
  :group 'enotify/tdd
  :type 'boolean)

(defun enotify/tdd::set-blink (slot-id)
  (when (gethash slot-id enotify/tdd::blink-table)
    (enotify/tdd::unset-blink slot-id))
  (let ((timer (run-with-timer enotify/tdd:blink-delay
                               enotify/tdd:blink-delay
                               'enotify/tdd::blink-helper slot-id))
        (original-face (enotify-face (getf (enotify-mode-line-notification slot-id) :face))))
    (puthash slot-id (list timer
                           original-face
                           original-face)
             enotify/tdd::blink-table)))

(defun enotify/tdd::unset-blink (slot-id)
  "Cancels the blinker timer and resets the notification original
face. Returns t if the notification was blinking, nil otherwise."
  (cond ((gethash slot-id enotify/tdd::blink-table)
         (destructuring-bind (timer original-face current-face)
             (gethash slot-id enotify/tdd::blink-table)
           (cancel-timer timer)
           (enotify-change-notification-face slot-id original-face))
         ;;(remhash slot-id enotify/tdd::blink-table)
         t)
        (t nil)))


(defun enotify/tdd:report-message-handler (slot-id data)
  (destructuring-bind (&key mode report-text message)
      data
    (let ((buf (get-buffer-create (enotify/tdd:test-result-buffer-name slot-id))))
      (with-current-buffer buf
        (erase-buffer)
        (insert report-text)
        (when (and message enotify/tdd:enable-message-notification)
          (message message))
        (when (and enotify/tdd:blink-delay (> enotify/tdd:blink-delay 0))
          (enotify/tdd::set-blink slot-id))
        (flet ((message (&rest args) (apply 'format args))
               ;; KLUDGE: this is needed for org mode: recentering is done on the current window!!
               (recenter (&rest args) nil))
          (if mode
              (funcall (intern (enotify/tdd::major-mode-fn mode)))
            (normal-mode)))))))


(defun enotify/tdd:mouse-1-handler (event)
  (interactive "e")
  (or (enotify/tdd::unset-blink (enotify-event->slot-id event))
      (enotify-change-notification-face (enotify-event->slot-id event) 'enotify/tdd::normal-face))
  (switch-to-buffer-other-window
   (enotify/tdd:test-result-buffer-name
    (enotify-event->slot-id event))))

(enotify/plugin:register
 "tdd"
 :handler 'enotify/tdd:report-message-handler
 :mouse-1-handler 'enotify/tdd:mouse-1-handler)

(provide 'enotify-tdd)
