;;; enotify.el --- a TCP based notification system for the emacs modeline

;; Copyright (C) 2012  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

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

;; This package provides a notification area on the emacs modeline.
;; External programs can send notifications via TCP, using a simple
;; sexp based protocol. See the README.md file for details.

;;; Code:

(defgroup enotify-minor-mode nil
  "Display notifications on emacs' mode line."
  :group 'modeline)

(require 'enotify-mode-line)
(require 'enotify-network)

;;;###autoload
(define-minor-mode enotify-minor-mode
  "Toggle display of notifications in the mode line."
  :global t :group 'enotify-minor-mode
  (setq enotify-mode-line-string nil)
  (or global-mode-string (setq global-mode-string (list "")))
  (if (not enotify-minor-mode)
      (setq global-mode-string
	    (delq 'enotify-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'enotify-mode-line-string t)
    (enotify-init-network)
    (enotify-mode-line-update)))

(provide 'enotify)
;;; enotify.el ends here
