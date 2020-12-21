;;; my-config-feebleline.el --- feebleline              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package feebleline)

(require 'feebleline)

;; interval
(setq feebleline-timer-interval 0.5)

;; message
(my-with-package evil)
(require 'my-init-evil)
(require 'evil)

(setq feebleline-msg-functions
      `((,(lambda ()
            (substring-no-properties (or evil-mode-line-tag ""))))
        (,#'feebleline-line-number         :post "")
        (,#'feebleline-column-number       :pre ":" :fmt "%-2s")
        (,#'feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
        (,#'feebleline-file-modified-star  :face font-lock-warning-face :post "")
        (,#'feebleline-file-directory      :face feebleline-dir-face :pre " ")))


(provide 'my-config-feebleline)
;;; my-config-feebleline.el ends here
