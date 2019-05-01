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

(defcustom maple-scratch-number 10
  "The number of show list."
  :group 'maple-scratch
  :type 'number)

(defcustom maple-scratch-alist
  '(("Files"
     :action (cond ((bound-and-true-p ivy-mode) 'ivy-recentf)
                   ((bound-and-true-p helm-mode) 'helm-recentf)
                   (t recentf-open-files))
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
    ("quit"
     :action 'save-buffers-kill-terminal
     :desc "Quit Emacs"))
  "List of insert."
  :group 'maple-scratch
  :type '(list))

(defface maple-scratch-face '((t (:inherit font-lock-comment-face)))
  "Text show face."
  :group 'maple-scratch)

(defmacro maple-scratch-insert (&rest body)
  "Insert BODY with []."
  `(progn
     (insert (maple-scratch--text "["))
     ,@body
     (insert (maple-scratch--text "]\t"))))

(defmacro maple-scratch-dolist (spec &rest body)
  "Like dolist but get INDEX, SPEC &REST BODY."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(let ((num 0))
     (dolist ,(cdr spec)
       (let ((,(car spec) num))
         ,@body
         (setq num (+ num 1))))))

(defun maple-scratch--subseq (seq start end)
  "Slice SEQ with START END."
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun maple-scratch--text (text &optional face)
  "TEXT add FACE."
  (propertize text 'font-lock-face (or face 'maple-scratch-face)))

(defun maple-scratch--button (label action &optional face help)
  "Button LABEL ACTION &OPTIONAL FACE HELP."
  (insert-button (format "%s" label)
                 'action
                 `(lambda (_) (call-interactively (or (command-remapping ,action) ,action)))
                 'follow-link t
                 'face (or face 'font-lock-keyword-face)
                 'help-echo (or help label)))

(defun maple-scratch--item (source action)
  "Button SOURCE ACTION &OPTIONAL -FACE."
  (insert "\n")
  (maple-scratch-dolist (index item source)
    (maple-scratch-insert
     (maple-scratch--button index `(lambda ()(interactive) `(,action ,item))))
    (maple-scratch--button item `(lambda ()(interactive) (,action ,item)) 'font-lock-comment-face)
    (insert "\n")))

(defun maple-scratch--init(label action &optional desc)
  "Insert LABEL ACTION DESC."
  (maple-scratch-insert
   (maple-scratch--button label action))
  (maple-scratch--button (or desc label) action 'font-lock-comment-face)
  (insert "\n"))

(defun maple-scratch--init-with-source(label action source source-action desc)
  "Insert LABEL &KEY ACTION SOURCE SOURCE-ACTION DESC."
  (maple-scratch-insert
   (maple-scratch--button label action))
  (if source (eval `(maple-scratch--item (maple-scratch--subseq ,source 0 maple-scratch-number) ,source-action))
    (insert "\t" (maple-scratch--text desc)))
  (insert "\n"))

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
           (concat prefix "Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))))

(defun maple-scratch-default()
  "Insert default message."
  (maple-scratch-dolist (index args maple-scratch-alist)
    (cl-destructuring-bind (label &key action source source-action desc) args
      (if maple-scratch-source
          (maple-scratch--init-with-source label action source source-action desc)
        (maple-scratch--init index action desc)))))

(defun maple-scratch-init()
  "Init maple-scratch."
  (with-current-buffer (get-buffer-create maple-scratch-buffer)
    (insert (maple-scratch-fortune))
    (maple-scratch-default)
    (insert (maple-scratch-startup))))

(defun maple-scratch-previous-button ()
  "Previous button."
  (interactive)
  (move-beginning-of-line 1)
  (let ((btn (previous-button (point))))
    (if btn (goto-char (button-start btn))
      (goto-char (point-max))
      (maple-scratch-previous-button))))

(defun maple-scratch-next-button ()
  "Next button."
  (interactive)
  (move-end-of-line 1)
  (let ((btn (next-button (point))))
    (if btn (goto-char (button-start btn))
      (goto-char (point-min))
      (maple-scratch-next-button))))

(defvar maple-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'maple-scratch-next-button)
    (define-key map (kbd "C-p") #'maple-scratch-previous-button)
    (define-key map (kbd "C-q") #'save-buffers-kill-terminal)
    map) "Keymap of command `maple-scratch-mode'.")

;;;###autoload
(define-minor-mode maple-scratch-mode
  "maple-scratch-mode."
  :global nil
  :keymap maple-scratch-mode-map
  (when maple-scratch-mode (maple-scratch-init)))

(provide 'maple-scratch)
;;; maple-scratch.el ends here
