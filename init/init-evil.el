;;; init-evil.el --- evil                            -*- lexical-binding: t; -*-

;; Use this file to make loading order about evil declarative.
;; Some configuration of evil need to be set before loading evil,
;; so emacs configuration files requiring evil must load this file
;; before loading evil.

(require 'my-bootstrap)
(my-with-package evil)

;;;; configuration

;; evil-keybindings.el also changes initial state of some modes,
;; e.g. `ag-mode'.
(custom-set-variables
 ;; '(evil-want-integration nil)
 '(evil-want-keybinding nil)
 '(evil-want-C-w-delete nil)
 '(evil-want-C-w-in-emacs-state t))

;; undo system
(custom-set-variables
 '(evil-undo-system 'undo-tree))

;;;; enable minor-mode
(require 'evil)

(evil-mode 1)

;;;; evil-surround

(my-with-package evil-surround)
(require 'evil-surround)

(global-evil-surround-mode 1)


(provide 'init-evil)
;;; init-evil.el ends here
