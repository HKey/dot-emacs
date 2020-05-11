;;; init-evil.el --- evil                            -*- lexical-binding: t; -*-

;; Use this file to make loading order about evil declarative.
;; Some configuration of evil need to be set before loading evil,
;; so emacs configuration files requiring evil must load this file
;; before loading evil.

(require 'my-bootstrap)
(use-package evil)

;;;; configuration
(custom-set-variables '(evil-want-integration nil))

;;;; enable minor-mode
(require 'evil)

(evil-mode 1)


(provide 'init-evil)
;;; init-evil.el ends here
