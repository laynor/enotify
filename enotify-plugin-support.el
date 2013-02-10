;;; enotify-plugin-support.el --- Enotify plugin support code

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience

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

;; 

;;; Code:

;; ((name :handler-fn myplugin-handler-fn :mouse-1-handler mouse-1-handler) ...)
(defvar enotify/plugin::alist nil)

(defun enotify/plugin:name-p (name)
  (not (null (assoc name enotify/plugin::alist))))

(defalias 'enotify/plugin:plugin-loaded-p 'enotify/plugin:name-p
  "Returns t if a plugin named NAME is loaded, nil otherwise")

(defun enotify/plugin:handler (name)
  (getf (enotify/plugin::descriptor name) :handler))

(defun enotify/plugin:mouse-1-handler-fn (name)
  (getf (enotify/plugin::descriptor name) :mouse-1-handler))

(defun enotify/plugin::descriptor (name)
  (cdr (assoc name enotify/plugin::alist)))

(defun* enotify/plugin:register (name &key handler mouse-1-handler)
  (assert (functionp handler))
  (assert (functionp mouse-1-handler))
  (push (list name :handler handler :mouse-1-handler mouse-1-handler)
        enotify/plugin::alist))

(provide 'enotify-plugin-support)
;;; enotify-plugin-support.el ends here
