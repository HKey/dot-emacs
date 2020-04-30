;;; config-go-mode.el --- go-mode                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package go-mode)

(require 'go-mode)

;;;; gofmt

(defun my-gofmt-set-hook-before-save ()
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(add-hook 'go-mode-hook #'my-gofmt-set-hook-before-save)

;;;; eldoc

(add-hook 'go-mode-hook #'go-eldoc-setup)


(provide 'config-go-mode)
;;; config-go-mode.el ends here