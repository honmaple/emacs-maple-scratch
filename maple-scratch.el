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
  :group 'maple)

(defcustom maple-scratch-buffer "*scratch*"
  "Only insert message in *scratch*  buffer."
  :group 'maple-scratch
  :type 'string)

(defcustom maple-scratch-source nil
  "Whether insert source list."
  :group 'maple-scratch
  :type 'boolean)

(defcustom maple-scratch-center t
  "Whether make buffer message center."
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

(defcustom maple-scratch-banner
  '("      ___           ___           ___           ___       ___"
    "     /\\__\\         /\\  \\         /\\  \\         /\\__\\     /\\  \\"
    "    /::|  |       /::\\  \\       /::\\  \\       /:/  /    /::\\  \\"
    "   /:|:|  |      /:/\\:\\  \\     /:/\\:\\  \\     /:/  /    /:/\\:\\  \\"
    "  /:/|:|__|__   /::\\~\\:\\  \\   /::\\~\\:\\  \\   /:/  /    /::\\~\\:\\  \\"
    " /:/ |::::\\__\\ /:/\\:\\ \\:\\__\\ /:/\\:\\ \\:\\__\\ /:/__/    /:/\\:\\ \\:\\__\\"
    " \\/__/~~/:/  / \\/__\\:\\/:/  / \\/__\\:\\/:/  / \\:\\  \\    \\:\\~\\:\\ \\/__/"
    "       /:/  /       \\::/  /       \\::/  /   \\:\\  \\    \\:\\ \\:\\__\\"
    "      /:/  /        /:/  /         \\/__/     \\:\\  \\    \\:\\ \\/__/"
    "     /:/  /        /:/  /                     \\:\\__\\    \\:\\__\\"
    "     \\/__/         \\/__/      EMACS ♥ YOU!     \\/__/     \\/__/")
  "The banner of show list."
  :group 'maple-scratch
  :type 'list)

(defcustom maple-scratch-write-mode 'emacs-lisp-mode
  "The default mode when writable."
  :group 'maple-scratch
  :type 'function)

(defcustom maple-scratch-items '(maple-scratch-fortune
                                 maple-scratch-banner
                                 maple-scratch-navbar
                                 maple-scratch-default
                                 maple-scratch-startup)
  "The item of show list."
  :group 'maple-scratch
  :type 'list)

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
     :desc "Open Bookmark")
    ("quit"
     :action 'save-buffers-kill-terminal
     :desc "Quit Emacs"))
  "List of insert."
  :group 'maple-scratch
  :type '(list))

(defcustom maple-scratch-navbar-alist
  '(("HOME"
     :action (lambda() (find-file (expand-file-name "init.el" user-emacs-directory)))
     :desc "Browse home")
    ("CAPTURE"
     :action 'org-capture
     :desc "Open Org Capture")
    ("AGENDA"
     :action 'org-agenda
     :desc "Open Org Agenda")
    ("HELP"
     :action (lambda() (find-file (expand-file-name "README.org" user-emacs-directory)))
     :desc "Emacs Help"))
  "Nav list of insert."
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
  (mapcar 'key-description (where-is-internal action overriding-local-map)))

(defun maple-scratch--keymap (action)
  "Make keymap with ACTION."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      `(lambda (_)
         (interactive "e")
         (funcall (or (command-remapping ,action) ,action))))
    (define-key map (kbd "RET")
      `(lambda ()
         (interactive)
         (funcall (or (command-remapping ,action) ,action))))
    map))

(defun maple-scratch--button (label keymap &optional face help)
  "LABEL ACTION &OPTIONAL KEYMAP FACE HELP."
  (propertize
   (format "%s" label)
   'face (or face 'font-lock-keyword-face)
   'mouse-face 'highlight
   ;; use button propertize to use next-button and previous-button
   'help-echo help
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
                    index `(lambda() (funcall ,source-action ,item)) nil nil item)))
       (format "\t%s" (maple-scratch--button (or desc label) keymap 'font-lock-comment-face))))))

(defun maple-scratch-default()
  "Insert default message in scratch buffer."
  (maple-scratch-concat
    maple-scratch-alist
    (cl-destructuring-bind (label &key action source source-action desc) item
      (if maple-scratch-source
          (maple-scratch-item label action source source-action desc)
        (maple-scratch-item index action nil nil desc)))))

(defun maple-scratch-startup ()
  "Insert start message in scratch buffer."
  (maple-scratch--text
   (format "Emacs startup finished in %.2fms with %s packages"
           (* 1000.0 (float-time (time-subtract after-init-time before-init-time)))
           (length load-path))))

(defun maple-scratch-fortune(&optional prefix)
  "Insert fortune message with PREFIX."
  (let ((fortune (when (executable-find "fortune")
                   "fortune -a | fmt -80 -s"))
        (cowsay (when (executable-find "cowsay")
                  (cond ((executable-find "shuf")
                         "cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n")
                        ((executable-find "gshuf")
                         "cowsay -$(gshuf -n 1 -e b d g p s t w y) -f $(gshuf -n 1 -e $(cowsay -l | tail -n +2)) -n")))))
    (maple-scratch--text
     (format (concat prefix "%s\n\n")
             (replace-regexp-in-string
              "\\[[0-9]*m" "" ; remove chinese shell char
              (replace-regexp-in-string
               "\n" (concat "\n" prefix) ; comment each line
               (replace-regexp-in-string
                "\s*$" ""    ; remove spaces
                (replace-regexp-in-string
                 "\n$" ""    ; remove trailing linebreak
                 (cond ((and fortune cowsay)
                        (shell-command-to-string (format "%s | %s" fortune cowsay)))
                       (fortune
                        (shell-command-to-string fortune))
                       (t ""))))))))))

(defun maple-scratch-banner()
  "Insert banner message."
  (when maple-scratch-banner
    (maple-scratch--text
     (mapconcat 'identity maple-scratch-banner "\n"))))

(defun maple-scratch-navbar()
  "Insert navbar message."
  (mapconcat
   (lambda(item)
     (cl-destructuring-bind (label &key action desc) item
       (concat
        (maple-scratch--text "[")
        (maple-scratch--button label (maple-scratch--keymap action) nil desc)
        (maple-scratch--text "]"))))
   maple-scratch-navbar-alist " "))

(defun maple-scratch-init()
  "Init maple-scratch."
  (maple-scratch-with
    (insert (mapconcat
             (lambda(x) (if maple-scratch-center (maple-scratch--center (funcall x)) (funcall x)))
             maple-scratch-items "\n\n"))
    (read-only-mode 1)
    (when maple-scratch-center
      (add-hook 'window-size-change-functions #'maple-scratch--resize nil t))))

(defun maple-scratch--resize(&rest _)
  "Resize buffer content, Make it center."
  (interactive)
  (maple-scratch-with
    (let ((inhibit-read-only t)
          (content (buffer-string)))
      (erase-buffer)
      (insert (maple-scratch--center content)))))

(defun maple-scratch--center(content)
  "Make CONTENT center of buffer."
  (let ((max-len 0)
        (content (with-temp-buffer
                   (insert content)
                   (indent-rigidly (point-min) (point-max) (- (indent-rigidly--current-indentation (point-min) (point-max))))
                   (buffer-string))))
    (cl-loop for line in (split-string content "\n")
             do (let ((len (length line))) (when (> len max-len) (setq max-len len))))
    (mapconcat
     (lambda(s) (concat (make-string (ceiling (max 0 (- (window-width) max-len)) 2) ? ) s))
     (split-string content "\n") "\n")))

(defun maple-scratch-forward-item ()
  "Forward item."
  (interactive)
  (forward-button 1 t))

(defun maple-scratch-backward-item ()
  "Backward item."
  (interactive)
  (backward-button 1 t))

(defun maple-scratch-previous-item ()
  "Previous item."
  (interactive)
  (move-beginning-of-line 1)
  (maple-scratch-backward-item))

(defun maple-scratch-next-item ()
  "Next item."
  (interactive)
  (move-end-of-line 1)
  (maple-scratch-forward-item))

(defun maple-scratch-writable ()
  "Writable."
  (interactive)
  (maple-scratch-with
    (maple-scratch-mode -1)
    (when maple-scratch-empty (erase-buffer))
    (when maple-scratch-write-mode (funcall maple-scratch-write-mode))))

(defvar maple-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'maple-scratch-next-item)
    (define-key map (kbd "k") #'maple-scratch-previous-item)
    (define-key map (kbd "h") #'maple-scratch-backward-item)
    (define-key map (kbd "l") #'maple-scratch-forward-item)
    (define-key map (kbd "e") #'maple-scratch-writable)
    (define-key map (kbd "q") #'save-buffers-kill-terminal)
    map) "Keymap of command `maple-scratch-mode'.")

;;;###autoload
(define-minor-mode maple-scratch-mode
  "maple-scratch-mode."
  :keymap maple-scratch-mode-map
  (if maple-scratch-mode (maple-scratch-init)
    (read-only-mode -1)
    (remove-hook 'window-size-change-functions #'maple-scratch--resize)))

(provide 'maple-scratch)
;;; maple-scratch.el ends here
