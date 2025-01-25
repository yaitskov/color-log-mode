;;; color-log-mode.el --- Eval SGR codes in a log buffer  -*- lexical-binding: t -*-

;; Author: Daniil Iaitskov <dyaitskov@gmail.com>
;; Maintainer: Daniil Iaitskov <dyaitskov@gmail.com>
;; URL: https://github.com/yaitskov/color-log-mode
;; Version: 0.0.1
;; Keywords: files, text, tools
;; Package-Requires: ((emacs "29.4"))

;; The software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with request.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for applying `ansi-color-apply-on-region' to
;; all buffer content.  All buffer hooks are removed and buffer becomes immutable.
;; With default binding to ".log" files.
;; Log files in such format are produced by "cabal test" eg.

;;; Code:

(require 'ansi-color)

(defun color-log-mode-list-hook (hook)
  "List all HOOK functions."
  (let ((r nil))
    (run-hook-wrapped
     hook
     (lambda (f &rest _)
       (progn (setq r (cons f r)) nil)))
    r))

(defun color-log-mode-clear-hook-locally (hook)
  "Remove all funtions from HOOK in the current buffer."
  (mapc (lambda (f) (remove-hook hook f t)) (color-log-mode-list-hook hook)))

(defface color-log-mode-face
  '((t :foreground "red" :background "green"))
  "Face for color-log-mode line.")

(defvar color-log-mode-evaled-hook nil
  "Hook is run when all SGR code are expanded.")

(defgroup color-log-mode nil "."
  :group 'text)

(defcustom color-log-mode-big-file-size 10000000
  "Maximum file size in bytes to be processed.
In case of a bigger file the mode is not activated."
  :group 'color-log-mode
  :type 'int)

(define-minor-mode color-log-mode
  "Apply `ansi-color-apply-on-region' to whole buffer.

The mode does not have any shortcut binding."
  ;; lighter text does not support multiple faces (all chars are styled with the first face)
  :lighter  (:propertize " l" face color-log-mode-face)
  :after-hook
  (let ((bn (current-buffer)))
    (when (bound-and-true-p color-log-mode) ;; prevent looping when mode tries to disable itself
      (run-at-time ;; async hack because setting buffer-file-name to nil breaks
       0 nil
       (lambda ()
         (with-current-buffer bn
           (when (and (< (point-max) color-log-mode-big-file-size) (> (point-max) 5))
             (let ((bufsize (point-max)))
               (ansi-color-apply-on-region (point-min) (point-max))
               (when (< (point-max) bufsize) ;; at least 1 SGR code is removed
                 ;; coloring function changes content and emacs always prompts on buffer kill
                 (color-log-mode-clear-hook-locally 'kill-buffer-hook)
                 (setq buffer-file-name nil)
                 (read-only-mode))))
           (when (not buffer-read-only)
             (run-at-time 0 nil (lambda ()
                                  (when (buffer-live-p bn)
                                    (with-current-buffer bn (color-log-mode -1))
                                    (message "Auto disable color-log-mode")))))
           (run-hooks 'color-log-mode-evaled-hook)))))))

;;;###autoload(autoload 'color-log-mode "color-log-mode")
;;;###autoload(add-to-list 'auto-mode-alist '("\\.log$" . color-log-mode))

(provide 'color-log-mode)
;;; color-log-mode.el ends here
