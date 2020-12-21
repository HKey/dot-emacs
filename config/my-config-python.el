;;; my-config-python.el --- python                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'python)

;;;; jedi-core

;; NOTE: jedi needs to refer PYTHONUSERBASE environment variable if virtualenv
;; is installed at the user local directory.
;; So I should add PYTHONUSERBASE to `exec-path-from-shell-variables'.

(my-with-package jedi-core)

(require 'my-init-exec-path-from-shell)
;; (require 'jedi-core)

;; (add-hook 'python-mode-hook #'jedi:setup)

;;;; company

(my-with-package company)
(my-with-package company-jedi)

(require 'company)

(defun my-python-setup-company ()
  (setq-local company-backends `(,#'company-jedi ,@company-backends)))

(add-hook 'python-mode-hook #'my-python-setup-company)

;;;; flycheck

(my-with-package flycheck)
(require 'flycheck)

;; Enable python-mypy checker
(flycheck-add-next-checker 'python-pycompile 'python-mypy)

(defun my-python-setup-flycheck ()
  (flycheck-mode 1))

(add-hook 'python-mode-hook #'my-python-setup-flycheck)


(provide 'my-config-python)
;;; my-config-python.el ends here
