;;; init-configuration.el --- Package list and basic configuration  -*- lexical-binding: t; -*-

(require 'package)

(eval-and-compile
  ;; `package-initialize' is called by `init.el' or `byte-compile-init.el'
  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "`use-package' package is missing, install it?: ")
      (package-refresh-contents)
      (package-install 'use-package))))

(require 'use-package)

(eval-and-compile
  (setq use-package-always-defer t
        use-package-always-ensure t
        use-package-verbose t))


;;; basic configuration

(progn
  (when window-system
    (fringe-mode 0)))


;;; libraries

(use-package s)
(use-package f)
(use-package dash)

(require 's)
(require 'f)
(require 'dash)


;;; shell

(use-package shell
  :ensure nil
  :init
  ;; config is in config-shell.el
  (autoload 'my-new-shell "config-shell" nil t))


;;; winner-mode

(use-package winner
  :ensure nil
  :init
  (winner-mode 1))


;;; save-place

(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))


;;; clipboard for emacs --no-window-system

;; - Emacs - ArchWiki
;;   https://wiki.archlinux.jp/index.php/Emacs

;; NOTE: may not work on tramp, because the process will be run on remote.
(defun my-xclip-cut (text &optional _push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xclip" nil 0 nil
                         "-i" "-selection" "clipboard")))

(defun my-xclip-paste ()
  ;; without "-noutf8" option on my system, the output of xclip contains "^M"
  (let ((xclip-output (shell-command-to-string
                       "xclip -o -selection clipboard -noutf8")))
    (unless (string= (car kill-ring) xclip-output)
      xclip-output)))

(when (and (null window-system) (getenv "DISPLAY"))
  (setq interprogram-cut-function #'my-xclip-cut
        interprogram-paste-function #'my-xclip-paste))


;;; org-mode

(use-package org
  :config
  ;; use org-id
  (require 'org-id)

  ;; use org-tempo for old style template expansion
  (require 'org-tempo))


;;; org-agenda

(use-package org-agenda
  :ensure nil
  :config
  (add-hook 'org-agenda-mode-hook #'hl-line-mode))


;;; org-download

(use-package org-download
  :config
  (setq org-download-method 'attach))


;;; org-super-agenda

(use-package org-super-agenda
  :init
  (with-eval-after-load 'org-agenda
    (org-super-agenda-mode 1))
  :config
  (setq org-super-agenda-groups '((:auto-tags t))))


;;; c/c++

;; configurations are in config-cc-mode.el

(use-package irony)

(use-package company-irony)

(use-package flycheck-irony)


;;; rust

(use-package rust-mode)

(use-package racer)

(use-package flycheck-rust)


;;; elisp

(use-package comment-or-uncomment-sexp)

(use-package lisp-extra-font-lock)

(use-package paredit)


;;; ielm

(use-package ielm
  :ensure nil
  :bind (:map ielm-map
              ("C-j" . newline-and-indent))
  :config
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))


;;; dimmer

(use-package dimmer
  ;; in "emacs -nw", dimmer slows redisplay down
  :if window-system
  :init
  ;; A workaround: While using some dark themes, echo area messages get
  ;; too dark to read.
  ;; That can be prevented by delaying turning dimmer-mode on.
  (add-hook 'emacs-startup-hook #'dimmer-mode)
  :config
  (cl-callf append dimmer-exclusion-regexp-list
    (list (rx bos " *helm")             ; for helm
          (rx bos " *hkey helm")        ; for my helm command
          (rx bos "*elfeed-entry*" eos) ; for elfeed entry buffer
          ))
  dimmer-fraction 0.4)


;;; eyebrowse

(use-package eyebrowse
  :functions evil-define-key*
  :bind (:map eyebrowse-mode-map
              ("C-t n" . eyebrowse-next-window-config)
              ("C-t p" . eyebrowse-prev-window-config)
              ("C-t w" . eyebrowse-switch-to-window-config))
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-t"))
  (eyebrowse-mode 1)
  (with-eval-after-load 'evil
    ;; Disable default "C-t" in evil.
    (evil-define-key* 'normal 'global eyebrowse-keymap-prefix nil))
  :config
  (setq eyebrowse-wrap-around t))


;;; iflipb

(use-package iflipb
  :bind (:map global-map
              ("M-t" . iflipb-next-buffer)
              ("M-T" . iflipb-previous-buffer))
  :config
  ;; don't skip buffers that start with "*"
  (setq iflipb-ignore-buffers nil))


;;; themes

(use-package paper-theme)

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (setq spacemacs-theme-org-agenda-height nil
        spacemacs-theme-org-bold nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-priority-bold nil))


;;; sublimity

(use-package sublimity
  :init
  ;; use sublimity-attractive (auto-margin)
  (require 'sublimity-attractive)

  ;; `sublimity-mode' disables `auto-hscroll-mode', so I turn on it again
  ;; using sublimity's hook.
  (eval-and-compile
    (defun my-sublimity-enable-auto-hscroll-mode ()
      (setq auto-hscroll-mode t)))
  (add-hook 'sublimity-mode-hook #'my-sublimity-enable-auto-hscroll-mode)

  ;; enable the minor mode
  (sublimity-mode 1))


;;; helm

(use-package helm)

(use-package helm-files
  :ensure nil
  :config
  (setq
   ;; Show full path for file selection.
   ;; To toggle that, press "C-]".
   helm-ff-transformer-show-only-basename nil

   ;; Disable fuzzy matching.
   helm-ff-fuzzy-matching nil))


;;; ivy

(use-package ivy
  :bind (nil                            ; just for indentation
         :map ivy-minibuffer-map
         ("M-m" . ivy-immediate-done) ; (lookup-key ivy-minibuffer-map (kbd "C-M-j"))
         ("C-k" . delete-minibuffer-contents)
         :map ivy-switch-buffer-map
         ("C-k" . nil))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-extra-directories nil
        ivy-truncate-lines nil
        ;; stop starting completion with "^" with `org-refile', `woman'
        ;; and more
        ivy-initial-inputs-alist nil))


;;; posframe

(use-package posframe
  :config
  ;; prevent moving mouse cursor
  (setq posframe-mouse-banish nil))


;;; ivy-posframe

;; ivy-posframe moves the mouse cursor
;; (use-package ivy-posframe
;;   :init
;;   (ivy-posframe-mode 1)
;;   :config
;;   (setq ivy-posframe-display-functions-alist
;;         '((t . ivy-posframe-display-at-window-center))))


;;; js2-mode and tern

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  ;; indent width
  (setq-default js2-basic-offset tab-width))

(use-package tern
  :mode ("\\.tern-project\\'" . json-mode)
  :init
  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook (lambda () (tern-mode 1)))))

(use-package company-tern
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))


;;; evil

(use-package evil
  :config
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

  ;; key bindings
  :bind (:map
         evil-emacs-state-map
         ;; do not override C-z
         ("C-z" . nil)
         ;; same with other states
         ("<escape>" . evil-exit-emacs-state)

         :map
         evil-insert-state-map
         ;; do not override some keys
         ("C-v" . nil)
         ("C-k" . nil)
         ("C-o" . nil)
         ("C-r" . nil)
         ("C-e" . nil)
         ("C-y" . nil)
         ("C-x C-n" . nil)
         ("C-x C-p" . nil)
         ("C-t" . nil)
         ("C-d" . nil)
         ("C-a" . nil)
         ;; mozc-temp
         ("M-n" . mozc-temp-convert-dwim)
         ;; caseformat
         ("M-l" . caseformat-backward)

         :map
         evil-motion-state-map
         ;; page up, down
         ("SPC" . evil-scroll-page-down)
         ("S-SPC" . evil-scroll-page-up)
         ;; beginning, end of line
         ("gh" . evil-beginning-of-line)
         ("gl" . evil-end-of-line)
         ;; swap visual and physical of line moving
         ("j" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         ("gj" . evil-next-line)
         ("gk" . evil-previous-line)
         ;; avy
         ("t" . avy-goto-word-0)
         ("T" . avy-goto-line)
         ("f" . avy-goto-char)

         :map
         evil-normal-state-map
         ;; bm
         ("m" . nil)
         ("mm" . bm-toggle)
         ("mn" . bm-next)
         ("mp" . bm-previous)
         ("ms" . bm-show)
         ("ma" . bm-show-all)

         :map
         evil-visual-state-map
         ;; expand-region
         ;; NOTE: using text object instead is better than
         ;;       binding keys directly?
         ("u" . er/expand-region)
         ("U" . er/contract-region)

         :map
         evil-window-map
         ;; buf-move
         ("SPC h" . buf-move-left)
         ("SPC j" . buf-move-down)
         ("SPC k" . buf-move-up)
         ("SPC l" . buf-move-right)
         ;; winner
         ("C-u" . winner-undo)))


;;; evil-surround

(use-package evil-surround
  :init
  (with-eval-after-load 'evil
    (global-evil-surround-mode 1)))


;;; feebleline

(use-package feebleline
  :init
  (setq feebleline-timer-interval 0.5)
  (feebleline-mode 1)
  :config
  (with-eval-after-load 'evil
    (push (list (lambda () evil-mode-line-tag)) feebleline-msg-functions)))



(provide 'init-configuration)
;;; init-configuration.el ends here
