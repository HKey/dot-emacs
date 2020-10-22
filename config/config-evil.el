;;; config-evil.el --- evil                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package evil)
(my-with-package dash)

(require 'init-evil)
(require 'evil)
(require 'dash)
(require 'lib-util)

;;;; config

;; initial state

(setq evil-motion-state-modes nil
      evil-emacs-state-modes
      (-uniq (append evil-emacs-state-modes
                     '(
                       ;; builtin
                       dired-mode
                       image-mode
                       calendar-mode
                       eww-mode
                       grep-mode
                       ;; extension
                       ztree-mode
                       pomidor-mode
                       ag-mode
                       elfeed-show-mode
                       elfeed-search-mode
                       bm-show-mode
                       flycheck-error-list-mode
                       ag-mode
                       undo-tree-visualize-mode))))

;; cursor
(setq evil-insert-state-cursor '(hbar . 4)
      evil-emacs-state-cursor 'hollow
      evil-motion-state-cursor '(bar . 1))

;; do not show state tag in mode-line
(setq evil-mode-line-format nil)

;; enable crossing lines
(setq evil-cross-lines t)

;; use fine undo
(setq evil-want-fine-undo t)

;; completion

(my-with-package company)
(require 'company-dabbrev)

(setq evil-complete-previous-func
      (lambda (_)
        (let ((company-dabbrev-char-regexp
               ;; from `dabbrev-expand'
               "\\(?:\\sw\\|\\s_\\)+")
              (company-dabbrev-time-limit 0.5)
              (company-dabbrev-other-buffers 'all))
          (call-interactively #'company-dabbrev)))
      evil-complete-next-func
      (lambda (_) (call-interactively #'company-complete)))

;; do not jump across buffers
(setq evil-jumps-cross-buffers nil)

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
(my-with-package mozc-temp)
(my-with-package caseformat)
(my-with-package avy)
(my-with-package bm)
(my-with-package expand-region)

(require 'bm)
(require 'expand-region)
(require 'winner)

(my-define-key evil-emacs-state-map
  ;; do not override C-z
  "C-z" nil
  ;; same with other states
  "<escape>" #'evil-exit-emacs-state)

(my-define-key evil-insert-state-map
  ;; do not override some keys
  "C-v" nil
  "C-k" nil
  "C-o" nil
  "C-r" nil
  "C-e" nil
  "C-y" nil
  "C-x C-n" nil
  "C-x C-p" nil
  "C-t" nil
  "C-d" nil
  "C-a" nil
  ;; mozc-temp
  "M-n" #'mozc-temp-convert-dwim
  "<muhenkan>" #'mozc-temp-convert-dwim
  ;; caseformat
  "M-l" #'caseformat-backward)

(my-define-key evil-motion-state-map
  ;; page up, down
  "SPC" #'evil-scroll-page-down
  "S-SPC" #'evil-scroll-page-up
  ;; beginning, end of line
  "gh" #'evil-beginning-of-line
  "gl" #'evil-end-of-line
  ;; swap visual and physical of line moving
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  "gj" #'evil-next-line
  "gk" #'evil-previous-line
  ;; avy
  "t" #'avy-goto-word-0
  "T" #'avy-goto-char
  "f" #'avy-goto-line)

(my-define-key evil-normal-state-map
  ;; undo-tree
  "U" #'undo-tree-redo
  ;; bm
  "m" nil
  "mm" #'bm-toggle
  "mn" #'bm-next
  "mp" #'bm-previous
  "ms" #'bm-show
  "ma" #'bm-show-all)

(my-define-key evil-visual-state-map
  ;; expand-region
  ;; NOTE: using text object instead is better than
  ;;       binding keys directly?
  "u" #'er/expand-region
  "U" #'er/contract-region)

(my-define-key evil-window-map
  ;; winner
  "C-u" #'winner-undo)

(my-define-key evil-ex-completion-map
  "C-a" nil                       ; old: evil-ex-completion
  "C-b" nil                       ; old: move-beginning-of-line
  "C-f" nil                       ; old: evil-ex-search-command-window
  )

;;;;; eyebrowse

(my-with-package eyebrowse)
(require 'eyebrowse)
(require 'config-eyebrowse)

;; Disable default "C-t" in evil.
(evil-define-key* 'normal 'global (kbd "C-t") nil)

;;;;; "q" prefix

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

(my-with-package transient)

(require 'transient)
(require 'outline)

;; See `outline-mode-prefix-map' for what commands are provided.
(transient-define-prefix my-transient-outline-minor-mode ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  [["Go to"
    ("n" "next heading" outline-next-visible-heading)
    ("p" "previous heading" outline-previous-visible-heading)
    ("f" "forward same level" outline-forward-same-level)
    ("b" "backward same level" outline-backward-same-level)
    ("u" "parent" outline-up-heading)]
   ["Hide/show"
    ("a" "show all" outline-show-all)
    ("o" "hide other" outline-hide-other)
    ("c" "toggle children" outline-toggle-children)]
   ["Move"
    ("U" "move subtree up" outline-move-subtree-up)
    ("D" "move subtree down" outline-move-subtree-down)]]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])

(my-define-key evil-motion-state-map
  "qyo" #'my-transient-outline-minor-mode)

;;;;; flycheck

(my-with-package transient)
(my-with-package flycheck)

(require 'transient)
(require 'flycheck)

(transient-define-prefix my-transient-flycheck-mode ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  [["Action"
    ("n" "next error" flycheck-next-error)
    ("p" "previous error" flycheck-previous-error)
    ("w" "copy errors" flycheck-copy-errors-as-kill :transient nil)
    ("h" "display error" flycheck-display-error-at-point)
    ("e" "explain error" flycheck-explain-error-at-point)]
   ["Checker"
    ("s" "select checker" flycheck-select-checker :transient nil)
    ("?" "describe checker" flycheck-describe-checker :transient nil)
    ("x" "disable checker" flycheck-disable-checker :transient nil)]
   ["Flycheck"
    ("c" "check buffer" flycheck-buffer :transient nil)
    ("C" "clear errors" flycheck-clear :transient nil)
    ("l" "list errors" flycheck-list-errors :transient nil)]]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])

(my-define-key evil-motion-state-map "qyf" #'my-transient-flycheck-mode)

;;;;; projectile

(my-with-package transient)
(my-with-package projectile)

(require 'transient)
(require 'projectile)

(defun my-projectile-switch-project-with-magit (project)
  (interactive
   (list (completing-read
          "Project: "
          projectile-known-projects
          nil t)))
  (magit-status project))

(transient-define-prefix my-transient-projectile-mode ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  [["Buffer"
    ("b" "switch to buffer" projectile-switch-to-buffer :transient nil)
    ("I" "ibuffer" projectile-ibuffer :transient nil)]
   ["File"
    ("f" "find file" projectile-find-file :transient nil)
    ("d" "dired" projectile-dired :transient nil)
    ("r" "recentf" projectile-recentf :transient nil)]
   ["Project"
    ("p" "switch project" projectile-switch-project :transient nil)
    ("m" "switch and magit" my-projectile-switch-project-with-magit
     :transient nil)]
   ["Search"
    ("/" "ag"  projectile-ag :transient nil)]]
  ["Shell"
   ("x" "async command in root" projectile-run-async-shell-command-in-root
    :transient nil)
   ("s" "run shell" projectile-run-shell :transient nil)]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])

(my-define-key evil-normal-state-map "qp" #'my-transient-projectile-mode)


(provide 'config-evil)
;;; config-evil.el ends here
