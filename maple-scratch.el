;;; maple-scratch.el ---  scratch message configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/honmaple/emacs-maple-scratch

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; scratch message configuration.
;;

;;; Code:
(defgroup maple-scratch nil
  "Show recent files and projects in scratch buffer."
  :group 'maple-scratch)

(defcustom maple-scratch-buffer "*scratch*"
  "Only insert scratch message in buffer."
  :group 'maple-scratch
  :type 'string)

(defcustom maple-scratch-source nil
  "Whether insert source."
  :group 'maple-scratch
  :type 'boolean)

(defcustom maple-scratch-empty t
  "Whether empty buffer when make it writable."
  :group 'maple-scratch
  :type 'boolean)

(defcustom maple-scratch-number 10
  "The number of show list."
  :group 'maple-scratch
  :type 'number)

(defcustom maple-scratch-alist
  '(("Files"
     :action (cond ((bound-and-true-p counsel-mode) 'counsel-recentf)
                   ((bound-and-true-p helm-mode) 'helm-recentf)
                   (t 'recentf-open-files))
     :source
     (progn (unless recentf-mode (recentf-mode)) recentf-list)
     :source-action 'find-file-existing
     :desc "Open Recenf Files")
    ("Projects"
     :action 'projectile-switch-project
     :source
     (progn (unless projectile-mode (projectile-mode)) projectile-known-projects)
     :source-action 'projectile-switch-project-by-name
     :desc "Open Project")
    ("Bookmarks"
     :action 'bookmark-jump
     :source
     (progn (unless (featurep 'bookmark) (require 'bookmark)) (bookmark-all-names))
     :source-action 'bookmark-jump
     :desc "Jump to Bookmark")
    ("Agenda"
     :action 'org-agenda
     :desc "Open org Agenda")
    ("quit"
     :action 'save-buffers-kill-terminal
     :desc "Quit Emacs"))
  "List of insert."
  :group 'maple-scratch
  :type '(list))

(defface maple-scratch-face '((t (:inherit font-lock-comment-face)))
  "Text show face."
  :group 'maple-scratch)

(defmacro maple-scratch-with (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(with-current-buffer (get-buffer-create maple-scratch-buffer) ,@body))

(defmacro maple-scratch-concat (items &rest body)
  "ITEMS &REST BODY."
  (declare (indent 0) (debug t))
  `(mapconcat
    'identity
    (cl-loop for index from 0 for item in ,items collect ,@body) "\n"))

(defun maple-scratch--subseq (seq start end)
  "Slice SEQ with START END."
  (cl-subseq seq start (and (number-or-marker-p end) (min (length seq) end))))

(defun maple-scratch--text (text &optional face)
  "TEXT add FACE."
  (propertize text 'font-lock-face (or face 'maple-scratch-face)))

(defun maple-scratch--keybind (action)
  "Lookup ACTION keys dynamically."
  (mapconcat 'key-description (where-is-internal action overriding-local-map) ", "))

(defun maple-scratch--keymap (action)
  "Make keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      `(lambda (_)
         (interactive "e")
         (call-interactively (or (command-remapping ,action) ,action))))
    (define-key map (kbd "RET")
      `(lambda ()
         (interactive)
         (call-interactively (or (command-remapping ,action) ,action))))
    map))

(defun maple-scratch--button (label keymap &optional face)
  "LABEL ACTION &OPTIONAL KEYMAP FACE."
  (propertize
   (format "%s" label)
   'face (or face 'font-lock-keyword-face)
   'mouse-face 'highlight
   ;; use button propertize to use next-button and previous-button
   'button (cons (list t) keymap)
   'keymap keymap))

(defun maple-scratch-item(label action &optional source source-action desc)
  "LABEL ACTION &OPTIONAL SOURCE SOURCE-ACTION DESC."
  (let ((keymap (maple-scratch--keymap action)))
    (concat
     (maple-scratch--text "[")
     (maple-scratch--button label keymap)
     (maple-scratch--text "]")
     (if source
         (format "\n%s\n"
                 (maple-scratch-concat
                   (maple-scratch--subseq (eval `,source) 0 maple-scratch-number)
                   (maple-scratch-item
                    index `(lambda() (interactive) (funcall ,source-action ,item)) nil nil item)))
       (format "\t%s" (maple-scratch--button (or desc label) keymap 'font-lock-comment-face))))))

(defun maple-scratch-default()
  "Default."
  (maple-scratch-concat
    maple-scratch-alist
    (cl-destructuring-bind (label &key action source source-action desc) item
      (if maple-scratch-source
          (maple-scratch-item label action source source-action desc)
        (maple-scratch-item index action nil nil desc)))))

(defun maple-scratch-startup ()
  "Insert start message with FACE."
  (maple-scratch--text
   (format "\nEmacs startup finished in %.2fms with %s packages\n"
           (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))
           (length load-path))))

(defun maple-scratch-fortune(&optional prefix)
  "Insert fortune message with PREFIX."
  (maple-scratch--text
   (concat (when (executable-find "fortune")
             (format (concat prefix "%s\n\n")
                     (replace-regexp-in-string
                      "\\[[0-9]*m" "" ; remove chinese shell char
                      (replace-regexp-in-string
                       "\n" (concat "\n" prefix) ; comment each line
                       (replace-regexp-in-string
                        "\s*$" ""    ; remove spaces
                        (replace-regexp-in-string
                         "\n$" ""    ; remove trailing linebreak
                         (shell-command-to-string
                          "fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n")))))))
           (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n"))))

(defun maple-scratch-init()
  "Init maple-scratch."
  (maple-scratch-with
    (insert (maple-scratch-fortune))
    (insert "\n")
    (insert (maple-scratch-default))
    (insert "\n")
    (insert (maple-scratch-startup))
    (read-only-mode 1)))

(defun maple-scratch-previous-item ()
  "Previous item."
  (interactive)
  (move-beginning-of-line 1)
  (let ((btn (previous-button (point))))
    (if btn (goto-char (button-start btn))
      (goto-char (point-max))
      (maple-scratch-previous-item))))

(defun maple-scratch-next-item ()
  "Next item."
  (interactive)
  (move-end-of-line 1)
  (let ((btn (next-button (point))))
    (if btn (goto-char (button-start btn))
      (goto-char (point-min))
      (maple-scratch-next-item))))

(defun maple-scratch-writable ()
  "Writable."
  (interactive)
  (maple-scratch-with
    (read-only-mode -1)
    (when maple-scratch-empty (erase-buffer))
    (assq-delete-all 'maple-scratch-mode minor-mode-map-alist)))

(defvar maple-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'maple-scratch-next-item)
    (define-key map (kbd "C-p") #'maple-scratch-previous-item)
    (define-key map (kbd "C-q") #'save-buffers-kill-terminal)
    (define-key map (kbd "n") #'maple-scratch-next-item)
    (define-key map (kbd "p") #'maple-scratch-previous-item)
    (define-key map (kbd "e") #'maple-scratch-writable)
    (define-key map (kbd "q") #'save-buffers-kill-terminal)
    map) "Keymap of command `maple-scratch-mode'.")

;;;###autoload
(define-minor-mode maple-scratch-mode
  "maple-scratch-mode."
  :keymap maple-scratch-mode-map
  (when maple-scratch-mode (maple-scratch-init)))

(provide 'maple-scratch)
;;; maple-scratch.el ends here
