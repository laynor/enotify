;;; enotify.el --- a TCP based notification system for the emacs modeline

;; Copyright (C) 2012  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience
;; URL: https://github.com/laynor/enotify
;; Version: VERSION
;; Package-Requires: ()

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
;; To use it, just add the enotify directory to your load-path, or
;; enotify.el if you are using the single file release, and then
;;
;; Internal programs wanting to write on the notification area can use
;; the `enotify-notify' function.
;;
;; (require 'enotify)
;; (enotify-minor-mode 1)
;;
;; If you plan to run more emacs sessions, you will probably get
;; in trouble as the port used by enotify is already in use by
;; another emacs sessions.
;; You can wrap (enotify-minor-mode 1) in a condition-case form like
;; this one
;;
;; (condition-case err
;;     (enotify-minor-mode 1)
;;   (error (display-warning 'enotify
;; 			  (format "Cannot start Enotify: %s" err)
;; 			  :error)))
;;
;;; Code:

(eval-when-compile (require 'cl))
