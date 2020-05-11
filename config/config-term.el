;;; config-term.el --- term                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(require 'term)

;;;; scroll-margin

(defun my-term-no-scroll-margin ()
  (setq-local scroll-margin 0))

(add-hook 'term-mode-hook #'my-term-no-scroll-margin)

;;;; evil

(require 'init-evil)
(require 'evil)

;; use `term-raw-map' mainly on insert state
(evil-make-overriding-map term-raw-map 'insert)

(provide 'config-term)
;;; config-term.el ends here
