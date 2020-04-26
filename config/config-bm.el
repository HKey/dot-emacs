;;; config-bm.el --- bm                              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package bm)

(require 'bm)

;; key map
(define-key bm-show-mode-map (kbd "n") #'next-line)
(define-key bm-show-mode-map (kbd "p") #'previous-line)

;; evil
(require 'evil)

(evil-set-initial-state 'bm-show-mode 'emacs)


(provide 'config-bm)
;;; config-bm.el ends here
