;;; color-log-mode-test.el --- test color-log package  -*- lexical-binding: t -*-
(require 'color-log-mode)
(require 'color-log-mode-autoloads)
(require 'ert)
(require 'ert-async)
(require 'f)

;;; Code:

(ert-deftest-async eval-SGR-test (end)
  (letrec
      ((check-SGR-exanded
        (lambda ()
          (unwind-protect
              (let ((plain (with-current-buffer "l.txt" (buffer-string)))
                    (faced (with-current-buffer "l.log" (buffer-string))))
                (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
                (when (not (with-current-buffer "l.log" buffer-read-only))
                  (funcall end "Buffer for [l.log] is not in read only mode"))
                (if (equal plain faced)
                    (funcall end)
                  (funcall end (format "Expected:\n%s\nGot:\n%s\n" plain faced))))
            (kill-buffer "l.txt")
            (kill-buffer "l.log")))))
    (copy-file "./x.log" "l.log" t)
    (find-file "l.txt")
    (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
    (find-file "l.log")))

(ert-deftest-async skip-empty-file-test (end)
  (letrec ((log-file "no-existing.log")
           (check
            (lambda ()
              (unwind-protect
                  (progn
                    (remove-hook 'color-log-mode-evaled-hook check)
                    (when buffer-read-only
                      (funcall end (concat "Buffer for [" log-file "] is immutable")))
                    (funcall end))
                (kill-buffer (current-buffer))))))
    (when (file-exists-p log-file)
      (delete-file log-file))
    (add-hook 'color-log-mode-evaled-hook check)
    (find-file log-file)))

(ert-deftest-async skip-big-file-test (end)
  (letrec
      ((l-log "l.log")
       (override-file-limit
        (lambda () (set (make-local-variable 'color-log-mode-big-file-size) 3)))
       (check-SGR-exanded
        (lambda ()
          (unwind-protect
              (let ((expected (f-read-text "x.log"))
                    (notfaced (buffer-string)))
                (remove-hook 'color-log-mode-hook override-file-limit)
                (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
                (when buffer-read-only
                  (funcall end (concat "Buffer for [" l-log "] is immutable")))
                (if (equal notfaced expected)
                    (funcall end)
                  (funcall end (format "Expected:\n%s\nGot:\n%s\n" expected notfaced))))
            (kill-buffer (current-buffer))))))
    (copy-file "./x.log" l-log t)
    (find-file "l.txt")
    (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
    (add-hook 'color-log-mode-hook override-file-limit)
    (find-file l-log)))

(provide 'color-log-mode-test)
;;; color-log-mode-test.el ends here
