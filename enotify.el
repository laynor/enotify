;;; enotify.el --- A networked notification system for Emacs

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enotify provides some sort of system tray for Emacs.
;;
;; It is born as a vehicle for TDD notifications, as the writer does
;; not like annoying popups floating around his tiled workspace.
;;
;; An application can connect to enotify and send a notification
;; message, that will discreetly appear in the Emacs mode-line.

;; The source code is available at https://github.com/laynor/enotify

;; INSTALLATION
;; ------------

;; Get the code, add the enotify directory to your Emacs load-path and require enotify:

;;     (add-to-list 'load-path "path/to/enotify")
;;     (require 'enotify)
;;     (enotify-minor-mode t)

;; If you customized the port number related variables (namely
;; `enotify-port', `enotify-use-next-available-port',
;; `enotify-fallback-ports'), ensure that the form
;;
;;     (enotify-minor-mode t)
;;
;; gets evaluated AFTER your customizations, or else your changes
;; won't affect enotify's startup.

;; USAGE
;; -----

;; Enotify uses the TCP port 5000 by default. You can customize
;; `enotify-default-port' if you want.  The variable
;; `enotify-use-next-available-port' contains a list of ports to be used
;; as a fallback when binding `enotify-default-port' fails.

;; It is also possible to instruct enotify to try increasing port numbers
;; (starting from `enotify-default-port' or the last port specified in
;; `enotify-fallback-ports' if this s available) for a fixed number of
;; times or indefinately until port 65535.  This is done throug the
;; custom variable `enotify-use-next-available-port'.

;; The command `enotify-port' displays what port enotify is currently
;; running on in the message area.

;; Messages are sent as strings and have this format:
;;
;;     "|<message size>|<message body>"
;;
;;
;; Message bodies have the form of a keyword argument list, like
;;
;; 	(:register "MySlotID")
;;
;; Example Message Sent through the network: "|22|(:register \"MySlotID\")"

;; The message size is intended as the length in characters of the message body.

;; Enotify slots
;; .............

;; An application that wants to send notifications should register a
;; `slot', - the equivalent of an icon in a system tray - before
;; sending any notification message.

;; The message used to register a slot has this form:
;;
;; 	(:register <slot-name> :handler-fn <message-data-processing-funcion>)

;; The function passed as :handler-fn is of the form
;; 	(handler-fn slot-id data)
;; The purpose of the :handler-fn parameter will be clarified in the
;; following section.

;; Notifications
;; .............

;; The message used to send a notification has the form

;; 	(:id <slot-name>
;; 	 :notification (:text <slot text>
;; 	                :face <slot face>
;; 			:help <tooltip text>
;; 			:mouse-1 <mouse-1 handler>)
;; 	 :data <additional-data>)

;; - data: it will be passed to the handler function specified in the
;;         slot registration message
;; - text: the text to be displayed in the notification area
;; - face: the face used to display the text in the notification area
;; - help: tooltip text on mouse-over
;; - mouse-1: an (iteractive "e") handler function of the form
;;
;;	        (m1-handler event)
;;
;;      that will be called when the user presses the left mouse button
;;      on the notification text.
;;      Inside the handler function it's possible to retrieve the slot id with
;;         (enotify-event->slot-id event)
;;
;; esend.sh
;; --------
;; The esend.sh script can be found in Enotify's git repository.
;; It's a small shell script that will send a message to enotify,
;; automatically calculating the message length.
;; It can be used to send simple notifications to emacs from shell scripts,
;; and provides a simple example of how enotify can be used.

;;; Code:

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
;;; enotify.el ends here
