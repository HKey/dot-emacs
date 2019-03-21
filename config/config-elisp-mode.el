;;; config-elisp-mode.el --- Emacs lisp             -*- lexical-binding: t; -*-

(require 'elisp-mode)
(require 'paredit)
(require 'lisp-extra-font-lock)
(require 'comment-or-uncomment-sexp)

;; hook
(defun my-emacs-lisp-mode-hook ()
  ;; some built-in emacs lisp files rely on that `tab-width' is set to 8.
  (setq tab-width 8)
  (enable-paredit-mode))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;; key binding
(define-key emacs-lisp-mode-map (kbd "C-;") #'comment-or-uncomment-sexp)

;; lisp-extra-font-lock
(lisp-extra-font-lock-global-mode 1)


(provide 'config-elisp-mode)
;;; config-elisp-mode.el ends here
