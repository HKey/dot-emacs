;;; config-evil.el --- evil                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package evil)

(require 'evil)
(require 'lib-util)

;;;; symbol motion

;; - Evil: EmacsをVimのごとく使う - 拡張編 - 貳佰伍拾陸夜日記
;;   http://d.hatena.ne.jp/tarao/20130305/evil_ext#tutorial2

(evil-define-motion my-evil-forward-symbol-begin (count)
  :type exclusive
  (evil-move-beginning count #'forward-evil-symbol))

(evil-define-motion my-evil-backward-symbol-begin (count)
  :type exclusive
  (evil-move-beginning (- (or count 1)) #'forward-evil-symbol))

(evil-define-motion my-evil-forward-symbol-end (count)
  :type inclusive
  (evil-move-end count #'forward-evil-symbol nil t))

(evil-define-motion my-evil-backward-symbol-end (count)
  :type inclusive
  (evil-move-end (- (or count 1)) #'forward-evil-symbol nil t))

;;;; key binding

(my-with-package undo-tree)

(my-define-key evil-normal-state-map "U" #'undo-tree-redo)

(my-define-key evil-ex-completion-map
  "C-a" nil                       ; old: evil-ex-completion
  "C-b" nil                       ; old: move-beginning-of-line
  "C-f" nil                       ; old: evil-ex-search-command-window
  )

;;;;; "q" prefix

(my-with-package org-taskforecast)
(my-with-package counsel)
(my-with-package swiper)
(my-with-package ztree)
(my-with-package major-mode-hydra)

(require 'init-my-commands)
(require 'dired-x)

(my-define-key evil-normal-state-map
  "q" nil
  "qq" #'evil-record-macro)

;; Using `evil-motion-state-map' is to use in motion state and normal state.
(my-define-key evil-motion-state-map
  ;; qm*: major commands
  "qma"      #'org-agenda
  "qmt"      #'org-taskforecast-list
  "qmb"      #'switch-to-buffer
  "qmc"      #'org-capture
  "qmd"      #'dired-jump
  "qmg"      #'magit-status
  "qmh"      #'counsel-recentf
  "qmi"      #'counsel-imenu
  "qml"      #'align-regexp
  "qmq"      #'kill-this-buffer
  "qm SPC q" #'my-delete-buffer-file
  "qms"      #'swiper
  "qmw"      #'my-find-memo
  "qmx"      #'async-shell-command
  "qmz"      #'ztree-dir

  ;; qn*: narrowing commands
  "qnd" #'narrow-to-defun
  "qnp" #'narrow-to-page
  "qnq" #'widen
  "qnw" #'widen

  ;; qh*: help commands
  "qha" #'counsel-apropos
  "qhb" #'counsel-descbinds
  "qhf" #'counsel-describe-function
  "qhF" #'counsel-describe-face
  "qhl" #'counsel-find-library
  "qhv" #'counsel-describe-variable

  ;; qw*: window commands
  "qw" evil-window-map

  ;; qy*: hydra
  "qym" #'major-mode-hydra
  )

;;;;; outline-minor-mode

(my-with-package hydra)

(require 'hydra)
(require 'outline)

;; See `outline-mode-prefix-map' for what commands are provided.
(defhydra my-hydra-outline-minor-mode (:foreign-keys run)
  "Hydra for `outline-minor-mode'"
  ("n" outline-next-visible-heading "next heading"
   :column "Move cursor")
  ("p" outline-previous-visible-heading "previous heading")
  ("u" outline-up-heading "parent")
  ("f" outline-forward-same-level "forward same level")
  ("b" outline-backward-same-level "backward same level")

  ("c" outline-toggle-children "toggle children"
   :column "Hide/Show")
  ("a" outline-show-all "show all")
  ("o" outline-hide-other "hide other")

  ("U" outline-move-subtree-up "move subtree up"
   :column "Move subtree")
  ("D" outline-move-subtree-down "move subtree down")

  ("<escape>" nil "quit hydra"
   :column "quit")
  ("q" nil "quit hydra"))

(my-define-key evil-motion-state-map
  "qyo" #'my-hydra-outline-minor-mode/body)

;;;;; flycheck

(my-with-package hydra)
(my-with-package flycheck)

(require 'hydra)
(require 'flycheck)

(defhydra my-hydra-flycheck-mode ()
  "Hydra for `flycheck-mode'"
  ("c" flycheck-buffer "check buffer" :exit t
   :column "flycheck")
  ("C" flycheck-clear "clear errors" :exit t)
  ("l" flycheck-list-errors "list errors" :exit t)
  ("x" flycheck-disable-checker "disable flycheck" :exit t)

  ("n" flycheck-next-error "next error"
   :column "error")
  ("p" flycheck-previous-error "previous error")
  ("C-w" flycheck-copy-errors-as-kill "copy errors" :exit t)
  ("h" flycheck-display-error-at-point "display error")
  ("e" flycheck-explain-error-at-point "explain error")

  ("s" flycheck-select-checker "select checker" :exit t
   :column "checker")
  ("?" flycheck-describe-checker "describe checker" :exit t)

  ("<escape>" nil "quit hydra"
   :column "quit")
  ("q" nil "quit hydra"))

(my-define-key evil-motion-state-map
  "qyf" #'my-hydra-flycheck-mode/body)

;;;;; projectile

(my-with-package hydra)
(my-with-package projectile)

(require 'hydra)
(require 'projectile)

(defun my-projectile-switch-project-with-magit (project)
  (interactive
   (list (completing-read
          "Project: "
          projectile-known-projects
          nil t)))
  (magit-status project))

(defhydra my-hydra-projectile-mode ()
  "Hydra for `projectile-mode'"
  ("x" projectile-run-async-shell-command-in-root "async shell command" :exit t
   :column "command")
  ("s" projectile-run-shell "run shell" :exit t)

  ("b" projectile-switch-to-buffer "switch to buffer" :exit t
   :column "buffer")
  ("I" projectile-ibuffer "ibuffer" :exit t)

  ("f" projectile-find-file "find file" :exit t
   :column "file")
  ("d" projectile-dired "dired" :exit t)
  ("r" projectile-recentf "recentf" :exit t)
  ("p" projectile-switch-project "switch project" :exit t)
  ("m" my-projectile-switch-project-with-magit "switch and magit" :exit t)

  ("/" projectile-ag "ag" :exit t
   :column "search"))

(my-define-key evil-normal-state-map
  "qp" #'my-hydra-projectile-mode/body)


(provide 'config-evil)
;;; config-evil.el ends here
