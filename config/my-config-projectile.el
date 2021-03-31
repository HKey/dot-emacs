;;; my-config-projectile.el --- projectile              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package projectile)

(require 'projectile)

;;;; `projectile-completion-system'

;; use default to use ivy instead of ido
(setq projectile-completion-system 'default)

;;;; transient

(my-with-package transient)
(my-with-package magit)

(require 'transient)

(transient-define-prefix my-transient-projectile-mode ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  [["Buffer"
    ("b" "switch to buffer" projectile-switch-to-buffer :transient nil)
    ("i" "ibuffer" projectile-ibuffer :transient nil)]
   ["File"
    ("f" "find file" projectile-find-file :transient nil)
    ("d" "dired" projectile-dired :transient nil)
    ("r" "recentf" projectile-recentf :transient nil)]
   ["Search"
    ("/" "ag"  projectile-ag :transient nil)]]
  [["Project"
    ("p" "switch project" projectile-switch-project :transient nil)
    ("m" "magit" magit-status
     :transient nil)]
   ["Shell"
    ("x" "async command in root" projectile-run-async-shell-command-in-root
     :transient nil)
    ("s" "run shell" projectile-run-shell :transient nil)]]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])

;;;; `projectile-switch-project-action'

(setq projectile-switch-project-action #'my-transient-projectile-mode)


(provide 'my-config-projectile)
;;; my-config-projectile.el ends here
