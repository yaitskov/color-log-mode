;;; color-log-mode-test.el --- test color-log package  -*- lexical-binding: t -*-
(require 'color-log-mode)
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
            (if (equal plain faced)
                (funcall end)
              (funcall end (format "Expected:\n%s\nGot:\n%s\n" plain faced)))))))
    (copy-file "./x.log" "l.log" t)
    (find-file "l.txt")
    (add-hook 'color-log-mode-evaled-hook check-SGR-exanded)
    (find-file "l.log")))

(provide 'color-log-mode-test)
;;; color-log-mode-test.el ends here
