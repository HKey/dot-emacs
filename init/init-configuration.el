;;; init-configuration.el --- Package list and basic configuration  -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;; basic configuration

(progn
  (when window-system
    (fringe-mode 0)
    ;; `default-frame-alist'
    (setq default-frame-alist
          '((height . 35)
            (width . 80)
            (inhibit-double-buffering . t)))))


;;; customize

;; workaround to limit custom variables to save
(defun my-limited-custom-save-all (&rest _)
  "Limit customized values to save."
  (message "Saving customized is limited by `my-limited-custom-save-all'")
  (mapatoms
   (lambda (s)
     (unless (memq s '(safe-local-variable-values))
       (when (get s 'saved-value)
         (put s 'saved-value nil)
         (message "  ignored: %s" s))))))

(advice-add #'custom-save-all :before #'my-limited-custom-save-all)


;;; dired

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-AFhlv --time-style=long-iso"))


;;; image

(use-package image-file
  :ensure nil
  :config
  ;; add webp as one of extensions of image file
  (cl-assert (not (member "webp" image-file-name-extensions))
             "webp is already registered in `image-file-name-extensions'")
  (push "webp" image-file-name-extensions))


;;; recentf

(use-package recentf
  :ensure nil
  :init
  ;; to prevent reducing `recentf-list' before loading config.
  (setq recentf-max-saved-items nil)
  (recentf-mode 1)
  :config
  ;; config is in config-recentf.el
  (require 'config-recentf))


;;; eldoc

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0.2
        eldoc-echo-area-use-multiline-p t))


;;; whitespace

(use-package whitespace
  :ensure nil
  :init
  (global-whitespace-mode 1)
  :config
  (setq whitespace-display-mappings
        '((space-mark   ?　 [?□])
          ;; (newline-mark ?\n [?↩ ?\n])
          ;; (tab-mark     ?\t [?^ ?\t])
          ))

  ;; See `whitespace-style-value-list'
  (setq whitespace-style
        '(face
          tabs
          ;; spaces
          trailing
          ;; lines
          ;; lines-tail
          ;; newline
          ;; empty
          ;; indentation
          ;; indentation::tab
          ;; indentation::space
          ;; space-after-tab
          ;; space-after-tab::tab
          ;; space-after-tab::space
          ;; space-before-tab
          ;; space-before-tab::tab
          ;; space-before-tab::space
          ;; help-newline
          tab-mark
          space-mark
          ;; newline-mark
          )))


;;; shr

(use-package shr
  :ensure nil
  :config
  ;; for eww web browsing
  (setq shr-inhibit-images t
        shr-width 76
        shr-use-colors nil))


;;; url

(use-package url-cookie
  :ensure nil
  :config
  ;; for eww web browsing
  ;; do not accept cookies
  (setq url-cookie-untrusted-urls '(".*")))


;;; sh

(use-package sh-script
  :ensure nil
  :config
  (setq sh-basic-offset tab-width
        sh-basic-offset tab-width))


;;; prog-mode

(use-package prog-mode
  :ensure nil
  :config
  ;; eldoc
  (add-hook 'prog-mode-hook #'eldoc-mode)
  ;; rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  ;; highlight-parentheses
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  ;; highlight-numbers
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  ;; hs-minor-mode
  (add-hook 'prog-mode-hook #'hs-minor-mode))


;;; ruby-mode

(use-package ruby-mode
  :ensure nil
  :config
  (setq ruby-insert-encoding-magic-comment nil
        ruby-deep-indent-paren-style nil))


;;; s

(use-package s)


;;; f

(use-package f)


;;; dash

(use-package dash
  :config
  (dash-enable-font-lock))


;;; libraries

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


;;; autorevert

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-verbose nil))


;;; generic-x

(use-package generic-x
  :ensure nil
  :init
  (require 'generic-x))


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
  ;; on my system, the output of xclip contains "^M"
  (--> (shell-command-to-string "xclip -o -selection clipboard")
       (s-replace "" "" it)
       (unless (string= (car kill-ring) it)
         it)))

(when (and (null window-system) (getenv "DISPLAY"))
  (setq interprogram-cut-function #'my-xclip-cut
        interprogram-paste-function #'my-xclip-paste))


;;; major-mode-hydra

(use-package major-mode-hydra)


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


;;; company

(use-package company
  :init
  (global-company-mode 1)
  :bind (nil                            ; just for indentation
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . nil)
         ("C-s" . company-filter-candidates)
         ("M-n" . nil)           ; for mozc-temp
         ("M-p" . nil)           ; to keep consistency with "M-n"

         :map company-search-map
         ("C-n" . company-search-repeat-forward)
         ("C-p" . company-search-repeat-backward))
  :config
  (setq company-idle-delay nil
        company-selection-wrap-around t
        company-tooltip-limit 30
        ;; company-minimum-prefix-length 1
        ;; company-minimum-prefix-length nil
        ))

(use-package company-dabbrev
  :ensure nil
  :config
  (setq company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil))

(use-package company-dabbrev-code
  :ensure nil
  :config
  (setq company-dabbrev-code-other-buffers nil))


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
  ;; (add-hook 'emacs-startup-hook #'dimmer-mode)
  ;; (require 'dimmer)                     ; to load config section
  :config
  (cl-callf append dimmer-buffer-exclusion-regexps
    (list (rx bos " *helm")             ; for helm
          (rx bos " *hkey helm")        ; for my helm command
          (rx bos "*elfeed-entry*" eos) ; for elfeed entry buffer
          ))
  dimmer-fraction 0.4)


;;; golden-ratio

(use-package golden-ratio
  :init
  ;; use golden-ratio to visualize which window is selected
  (golden-ratio-mode 1)
  ;; selected window as the main window
  ;; disable this:
  ;;     (remove-hook 'window-selection-change-functions #'golden-ratio)
  (add-hook 'window-selection-change-functions #'golden-ratio))


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

(use-package sublimity-attractive
  :ensure nil
  :config
  (setq sublimity-attractive-centering-width 80))

(use-package sublimity
  :init
  ;; use sublimity-attractive (auto-margin)
  (require 'sublimity-attractive)

  ;; enable the minor mode
  (sublimity-mode 1)
  :config
  ;; config is in config-sublimity.el
  (require 'config-sublimity))


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
  ;; config is in config-ivy.el
  )


;;; ivy-prescient

(use-package ivy-prescient
  :after (ivy config-ivy)
  :init
  (require 'ivy-prescient)
  ;; to use `my-ivy-regex-plus-quoted' as a default re builder,
  ;; do not overwrite `ivy-re-builders-alist'.
  (setq ivy-prescient-enable-filtering nil)
  (ivy-prescient-mode 1))


;;; posframe

(use-package posframe
  :config
  ;; prevent moving mouse cursor
  (setq posframe-mouse-banish nil)
  ;; disable double buffering
  (setq posframe-inhibit-double-buffering t))


;;; ivy-posframe

(use-package ivy-posframe
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist
        ;; '((t . ivy-posframe-display-at-window-center))
        '((t . ivy-posframe-display-at-frame-center))
        ;; width limitation is needed to work with sublimity-attractive
        ivy-posframe-min-height 10
        ivy-posframe-width 70
        ivy-posframe-min-width 70
        ivy-posframe-border-width 2))


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


;;; org-taskforecast

(use-package org-taskforecast
  :ensure nil
  :config
  (setq org-taskforecast-enable-assert t
        org-taskforecast-day-start 0400
        org-taskforecast-sections '(("0700" 0700)
                                    ("0900" 0900)
                                    ("1200" 1200)
                                    ("1300" 1300)
                                    ("1500" 1500)
                                    ("1700" 1700)
                                    ("1900" 1900)
                                    ("2100" 2100)
                                    ("2300" 2300)
                                    ("2630" 2630)))
  (use-package org-agenda
    :ensure nil
    :config
    (define-key org-agenda-mode-map (kbd "r") #'org-taskforecast-register-task))
  (add-hook 'org-taskforecast-list-mode-hook #'hl-line-mode))


;;; mozc

(use-package mozc
  :bind (nil                            ; just for indentation
         :map global-map
         ("<zenkaku-hankaku>" . toggle-input-method)
         ("<muhenkan>" . toggle-input-method)
         :map mozc-mode-map
         ("<muhenkan>" . toggle-input-method))
  :config
  (setq default-input-method "japanese-mozc"
        ;; - `echo-area'
        ;; - `overlay'
        mozc-candidate-style 'echo-area))


;;; mozc-temp

(use-package mozc-temp
  :config
  (setq mozc-temp-auto-conversion t))


;;; undo-tree

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :bind (:map global-map
              ("C-z"   . undo-tree-undo)
              ("C-S-z" . undo-tree-redo)
              ("M-z"   . undo-tree-visualize)))


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

  ;; do not jump across buffers
  (setq evil-jumps-cross-buffers nil)

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
         ("T" . avy-goto-char)
         ("f" . avy-goto-line)

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

;;; projectile

(use-package projectile
  :init
  (projectile-mode 1)
  :config
  ;; use default to use ivy instead of ido
  (setq projectile-completion-system 'default))


;;; which-key

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-max-description-length nil
        which-key-side-window-max-height 0.5))


;;; avy

(use-package avy
  :config
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
   avy-style 'words))


;;; flycheck

(use-package flycheck
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        ;; run flycheck manually
        flycheck-check-syntax-automatically nil))


;;; highlight-numbers

(use-package highlight-numbers
  ;; used by `prog-mode'
  )


;;; flycheck-posframe

(use-package flycheck-posframe
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  (set-face-attribute 'flycheck-posframe-border-face nil :inherit 'default)
  (setq flycheck-posframe-border-width 2))


;;; slime

(use-package slime
  :config
  (setq inferior-lisp-program "ccl"
        slime-lisp-implementations '((clozurecl ("ccl"))
                                     (sbcl      ("sbcl"))
                                     (clisp     ("clisp"))
                                     (ecl       ("ecl")))
        slime-net-coding-system 'utf-8-unix)
  (--each '(slime-mode-hook slime-repl-mode-hook)
    (add-hook it #'enable-paredit-mode)))



(provide 'init-configuration)
;;; init-configuration.el ends here
