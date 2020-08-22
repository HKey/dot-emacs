;;; config-avy.el --- avy                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package avy)

(require 'avy)

(setq
 ;; dvorak
 avy-keys (cl-coerce "iduhetonasyfpg.c,r'lxbkmjwqv;z" 'list)
 ;; t: make background characters gray
 avy-background nil
 ;; use current window only
 avy-all-windows nil
 ;; with C-u prefix, use all windows
 avy-all-windows-alt t
 ;; words is good to type, de-bruijn is also good
 avy-style 'words)


(provide 'config-avy)
;;; config-avy.el ends here
