;;; my-config-elisp-mode.el --- Emacs lisp             -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package paredit)
(my-with-package lisp-extra-font-lock)

(require 'elisp-mode)
(require 'paredit)
(require 'lisp-extra-font-lock)

;; hook
(defun my-emacs-lisp-mode-hook ()
  ;; some built-in emacs lisp files rely on that `tab-width' is set to 8.
  (setq tab-width 8)
  (enable-paredit-mode))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;; lisp-extra-font-lock
(lisp-extra-font-lock-global-mode 1)

;;;; linter

(my-with-package package-lint)


(provide 'my-config-elisp-mode)
;;; my-config-elisp-mode.el ends here
