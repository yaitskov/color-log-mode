;;; color-log-mode-test.el --- test color-log package  -*- lexical-binding: t -*-
(require 'color-log-mode)
(require 'color-log-mode-autoloads)
(require 'ert)
(require 'ert-async)

;;; Code:

(ert-deftest-async eval-SGR-test (end)
  (letrec
      ((check-SGR-exanded
        (lambda ()
          (let ((plain (with-current-buffer "l.txt" (buffer-string)))
                (faced (with-current-buffer "l.log" (buffer-string))))
            (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
            (when (not (with-current-buffer "l.log" buffer-read-only))
              (funcall end "Buffer for [l.log] is not in read only mode"))
            (if (equal plain faced)
                (funcall end)
              (funcall end (format "Expected:\n%s\nGot:\n%s\n" plain faced)))))))
    (copy-file "./x.log" "l.log" t)
    (find-file "l.txt")
    (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
    (find-file "l.log")))

(ert-deftest-async skip-empty-file-test (end)
  (letrec ((log-file "no-existing.log")
           (check
            (lambda ()
              (remove-hook 'color-log-mode-evaled-hook check)
              (when (with-current-buffer log-file buffer-read-only)
                (funcall end (concat "Buffer for [" log-file "] is immutable")))
              (funcall end))))
    (when (file-exists-p log-file)
      (delete-file log-file))
    (add-hook 'color-log-mode-evaled-hook check)
    (find-file log-file)))

(provide 'color-log-mode-test)
;;; color-log-mode-test.el ends here
