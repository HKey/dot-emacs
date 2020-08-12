;;; config-paredit.el --- paredit                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package paredit)

(require 'paredit)
(require 'lib-util)

;;;; key binding

(my-define-key paredit-mode-map
  [remap delete-backward-char] #'paredit-backward-delete
  "M-q" nil                             ; Use default M-q.
  )

;;;; evil

(require 'init-evil)
(require 'evil)

(evil-make-overriding-map paredit-mode-map 'insert)

(my-define-key paredit-mode-map
  [remap evil-delete-char]                   #'paredit-forward-delete
  [remap evil-delete-backward-char]          #'paredit-backward-delete
  [remap evil-delete-backward-char-and-join] #'paredit-backward-delete
  [remap evil-delete-line]                   #'paredit-kill)


(provide 'config-paredit)
;;; config-paredit.el ends here
