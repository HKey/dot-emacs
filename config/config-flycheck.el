;;; config-flycheck.el --- flycheck                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package flycheck)

(require 'flycheck)

;;; config

(setq flycheck-emacs-lisp-load-path 'inherit
      ;; run flycheck manually
      flycheck-check-syntax-automatically nil)

;;; flycheck-posframe

(my-with-package flycheck-posframe)

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)


(provide 'config-flycheck)
;;; config-flycheck.el ends here
