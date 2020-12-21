;;; my-config-cc-mode.el --- c/c++                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package dash)

(require 'cc-mode)
(require 'dash)

;;;; indentation
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-intro '+)

;;;; hooks
(defun my-c/c++-mode-hook ()
  ;; flycheck
  (flycheck-mode 1))

(--each '(c-mode-hook c++-mode-hook)
  (add-hook it #'my-c/c++-mode-hook))


(provide 'my-config-cc-mode)
;;; my-config-cc-mode.el ends here
