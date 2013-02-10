;;;; Espectator plugin for enotify
(require 'enotify)

(defun enotify/tdd:test-result-buffer-name (slot-id)
  (format "*%s test results*" slot-id))

(defun enotify/tdd::major-mode-fn (mode-name)
  (concat mode-name "-mode"))

(make-face 'enotify/tdd::normal-face)

(defvar enotify/tdd::blink-face-table (make-hash-table :test 'equal))

(defun enotify/tdd::register-blink (slot-id face timer)
  (puthash slot-id
           (list (face-foreground face) (face-background face) timer blinker-face)
           enotify/tdd::blink-face-table))

(defun enotify/tdd::unregister-blink (slot-id)
  (when (gethash slot-id enotify/tdd::blink-face-table)
    (destructuring-bind (fg bg timer blinker-face)
        (gethash slot-id enotify/tdd::blink-face-table)
      (set-face-foreground blinker-face fg)
      (set-face-background blinker-face bg)
      (cancel-timer timer))
    (remhash slot-id enotify/tdd::blink-face-table)))

(defun enotify/tdd::toggle-face-foreground-helper (slot-id face alt-color)
  (destructuring-bind (fg bg timer blinker-face)
      (gethash slot-id enotify/tdd::blink-face-table)
    (if (equal (face-foreground face) fg)
        (set-face-foreground face alt-color)
      (set-face-foreground face fg))))

(defun enotify/tdd::set-face-blink (face slot-id blink-delay alt-color)
  (let ((face (enotify-face face)))
    (unless (gethash face enotify/tdd::blink-face-table)
      (let ((blinker-face (make-face (intern (concat "enotify/tdd::blinker-face-" slot-id)))))
        (let((timer (run-with-timer blink-delay blink-delay
                                    'enotify/tdd::toggle-face-foreground-helper slot-id blinker-face alt-color)))
          (apply 'set-face-attribute blinker-face nil (face-attr-construct face))
          (enotify-change-notification-face slot-id blinker-face)
          (enotify/tdd::register-blink slot-id face timer))))))

(defun enotify/tdd:report-message-handler (slot-id data)
  (destructuring-bind (&key mode report-text)
      data
    (let ((buf (get-buffer-create (enotify-rspec-result-buffer-name slot-id))))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer)
        (insert report-text)
        (enotify/tdd::set-face-blink (enotify-face (getf (enotify-mode-line-notification slot-id) :face))
                                     slot-id 1 (face-foreground 'enotify/tdd::normal-face))
        (cl-flet ((message (&rest args) (apply 'format args)))
          (funcall (intern (enotify/tdd::major-mode-fn mode))))))))

(defun enotify/tdd:mouse-1-handler (event)
  (interactive "e")
  (enotify-change-notification-face (enotify-event->slot-id event) 'enotify-normal-face)
  (switch-to-buffer-other-window
   (enotify-rspec-result-buffer-name
    (enotify-event->slot-id event))))

(enotify/plugin:register
 "tdd"
 :handler 'enotify/tdd:report-message-handler
 :mouse-1-handler 'enotify/tdd:mouse-1-handler)

(provide 'enotify-espectator)
