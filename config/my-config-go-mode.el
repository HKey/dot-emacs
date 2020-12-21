;;; my-config-go-mode.el --- go-mode                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package go-mode)

(require 'go-mode)

;;;; gofmt

(defun my-gofmt-set-hook-before-save ()
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(add-hook 'go-mode-hook #'my-gofmt-set-hook-before-save)

;;;; eldoc

(my-with-package go-eldoc)

(add-hook 'go-mode-hook #'go-eldoc-setup)


(provide 'my-config-go-mode)
;;; my-config-go-mode.el ends here
