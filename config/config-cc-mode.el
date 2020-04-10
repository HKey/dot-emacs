;;; config-cc-mode.el --- c/c++                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'cc-mode)
(require 'dash)
(require 'company)

;; indentation
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-cont-nonempty '+)
(c-set-offset 'arglist-intro '+)

(defun my-c/c++-mode-hook ()
  ;; irony
  (irony-mode 1)

  ;; company-irony
  (make-local-variable 'company-backend)
  (push 'company-irony company-backends)

  ;; flycheck
  (flycheck-mode 1)

  ;; flycheck-irony
  (flycheck-irony-setup))

(--each '(c-mode-hook c++-mode-hook)
  (add-hook it #'my-c/c++-mode-hook))


(provide 'config-cc-mode)
;;; config-cc-mode.el ends here
