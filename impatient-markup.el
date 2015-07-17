;;; impatient-markup.el --- 
;; 
;; Filename: impatient-markup.el
;; Description: 
;; Author: anand
;; Maintainer: 
;; Created: Tue Jun 16 18:25:56 2015 (+0530)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'impatient-mode)

(defvar impatient-markup-serve-at-start t
  "Open live html when markup buffer is opened.")

(defvar impatient-markup-pandoc "pandoc"
  "Path to pandoc executable.")

(defvar impatient-markup-html-buffer "impatient-markup.html"
  "Temporary buffer to hold html.")

(defvar impatient-markup-buffer nil
  "Buffer whose contents to be converted to html")

(defvar impatient-markup-server-url "http://127.0.0.1:8080/imp/live/"
  "Default prefix url of impatient-mode.")


(defun impatient-markup-serve-file ()
  "Activate impatient mode and open url in a browser."
  (with-current-buffer (get-buffer-create impatient-markup-html-buffer)
    (funcall 'html-mode)
    (impatient-mode))
  (browse-url (concat impatient-markup-server-url impatient-markup-html-buffer)))


(defun impatient-markup-update (&rest args)
  "Update html buffer if markup buffer updates."
  (save-buffer impatient-markup-buffer)
  (with-current-buffer (get-buffer impatient-markup-html-buffer)
    (erase-buffer)
    (let ((inhibit-modification-hooks nil))
      (insert (shell-command-to-string 
               (format "%s %s" impatient-markup-pandoc impatient-markup-buffer))))))


(define-minor-mode impatient-markup
  "Serves markup as live html over HTTP."
  :group 'im
  :lighter " im"
  (if (buffer-file-name)
      (progn
        (setq impatient-markup-buffer (buffer-file-name))
        (with-current-buffer (current-buffer)
          (add-hook 'after-change-functions 'impatient-markup-update nil t))
        ;; (impatient-markup-update nil)
        (impatient-markup-serve-file)
        )))


(defun impatient-markup-enable ()
  "Enable impatient-markup mode."
  ;; (interactive)
  (httpd-start)
  (add-hook 'markdown-mode-hook 'impatient-markup)
  (add-hook 'rst-mode-hook 'impatient-markup))

;; (defun impatient-markup-disable ()
;;   "Disable impatient-markup mode."
;;   (interactive)

;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; impatient-markup.el ends here
