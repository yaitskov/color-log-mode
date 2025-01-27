;;; color-log-mode-test.el --- test color-log package  -*- lexical-binding: t -*-
(require 'color-log-mode)
(require 'color-log-mode-autoloads)
(require 'ert)
(require 'ert-async)
(require 'ert-scope)
(require 'f)

;;; Code:

(defmacro color-log-mode-test (cdir tdir end &rest body)
  "CDIR TDIR BODY.

END callback is provided by `ert-deftest-async'."
  `(ert-scope-with-temp-dir-async
     ,end ,cdir ,tdir
     (ert-scope-buffers-async ,end ,@body)))

(ert-deftest-async eval-SGR-test (end)
  (color-log-mode-test
   cdir tdir end
   (letrec
       ((check-SGR-exanded
         (lambda ()
           (let ((plain (with-current-buffer "l.txt" (buffer-string)))
                 (faced (with-current-buffer "l.log" (buffer-string))))
             (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
             (when (not (with-current-buffer "l.log" buffer-read-only))
               (funcall end (concat "Buffer for [l.log] is not in read only mode: " (buffer-string))))
             (if (equal plain faced)
                 (funcall end)
               (funcall end (format "Expected:\n%s\nGot:\n%s\n" plain faced)))))))
     (copy-file (concat cdir "/x.log") (concat tdir "/l.log") t)
     (find-file (concat cdir "/l.txt"))
     (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
     (find-file (concat tdir "/l.log")))))

(ert-deftest-async skip-empty-file-test (end)
  (color-log-mode-test
   cdir tdir end
   (letrec ((log-file "no-existing.log")
            (check
             (lambda ()
               (remove-hook 'color-log-mode-evaled-hook check)
               (when buffer-read-only
                 (funcall end (concat "Buffer for [" log-file "] is immutable")))
               (funcall end))))
     (add-hook 'color-log-mode-evaled-hook check)
     (find-file (concat tdir "/" log-file)))))

(ert-deftest-async skip-big-file-test (end)
  (color-log-mode-test
   cdir tdir end
   (letrec
       ((l-log "l.log")
        (override-file-limit
         (lambda () (set (make-local-variable 'color-log-mode-big-file-size) 3)))
        (check-SGR-exanded
         (lambda ()
           (let ((expected (f-read-text (concat cdir "/x.log")))
                 (notfaced (buffer-string)))
             (remove-hook 'color-log-mode-hook override-file-limit)
             (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
             (when buffer-read-only
               (funcall end (concat "Buffer for [" l-log "] is immutable")))
             (if (equal notfaced expected)
                 (funcall end)
               (funcall end (format "Expected:\n%s\nGot:\n%s\n" expected notfaced)))))))
     (copy-file (concat cdir "/x.log") (concat tdir "/" l-log) t)
     (find-file (concat cdir "/l.txt"))
     (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
     (add-hook 'color-log-mode-hook override-file-limit)
     (find-file (concat tdir "/" l-log)))))

(ert-deftest-async skip-file-without-any-SGR-test (end)
  (color-log-mode-test
   cdir tdir end
   (letrec
       ((check-SGR-exanded
         (lambda ()
           (let ((plain (f-read-text (concat cdir "/l.txt")))
                 (notfaced (buffer-string)))
             (remove-hook 'color-log-mode-evaled-hook check-SGR-exanded)
             (when buffer-read-only
               (funcall end "Buffer for [l.log] is in read only mode"))
             (if (equal plain notfaced)
                 (funcall end)
               (funcall end (format "Expected:\n%s\nGot:\n%s\n%s" plain notfaced (buffer-name))))))))
     (copy-file (concat cdir "/l.txt") (concat tdir "/l.log") t)
     (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
     (find-file (concat tdir "/l.log")))))

(provide 'color-log-mode-test)
;;; color-log-mode-test.el ends here
