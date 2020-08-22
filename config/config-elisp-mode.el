;;; config-elisp-mode.el --- Emacs lisp             -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package paredit)
(my-with-package lisp-extra-font-lock)
(my-with-package comment-or-uncomment-sexp)
(my-with-package major-mode-hydra)

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

;; major-mode-hydra
(require 'hydra)   ; for byte compilation of `major-mode-hydra-define'
(require 'major-mode-hydra)

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("ed" eval-defun)
    ("eb" eval-buffer))
   "REPL"
   (("r" ielm))
   "Package"
   (("pf" package-install-file)
    ("pb" package-install-from-buffer))
   "Describe"
   (("df" describe-function)
    ("dv" describe-variable))))

;;;; linter

(my-with-package package-lint)
(my-with-package relint)


(provide 'config-elisp-mode)
;;; config-elisp-mode.el ends here
