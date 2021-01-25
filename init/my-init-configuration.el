;;; my-init-configuration.el --- Package list and basic configuration  -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;; basic configuration

;;;; frame-alist
;; `default-frame-alist'
(setq default-frame-alist
      '((height . 40)
        (width . 84)
        (inhibit-double-buffering . t)
        (internal-border-width . 10)
        ;; (right-fringe . 0)
        ;; (left-fringe . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (undecorated . t)))

;;;; supress same file warnings like "file-a and file-b are the same file"
(setq find-file-suppress-same-file-warnings t)

;;;; disable auto save files, backup files and lockfiles
(setq
 ;; delete auto save files before exitting
 delete-auto-save-files t
 ;; do not create auto save files
 auto-save-default nil
 ;; do not create backup files
 backup-inhibited t
 make-backup-files nil
 ;; do not create lockfiles
 create-lockfiles nil)

;;;; completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;; scrolling
(setq scroll-step 1
      scroll-conservatively 2
      ;; scroll-margin 2
      scroll-preserve-screen-position t
      hscroll-step 1
      hscroll-margin 30
      make-cursor-line-fully-visible t
      auto-window-vscroll nil
      ;; auto-hscroll-mode 'current-line
      )

(when (boundp 'fast-but-imprecise-scrolling)
  (setq fast-but-imprecise-scrolling t))

;;;; editing
(setq
 ;; kill whole line by pressing "C-k" once at BOL
 kill-whole-line t
 ;; always insert an empty line at EOF
 require-final-newline t)

;;;; initial messages
(setq inhibit-startup-message t)
;; (setq inhibit-splash-screen t)
;; (setq inhibit-startup-screen t)
;; (setq inhibit-startup-buffer-menu t)

;;;; bell
;; - Emacs Lisp TIPS
;;   https://sci.nao.ac.jp/MEMBER/zenitani/elisp-j.html#bell
(setq ring-bell-function #'ignore)

;;;; garbage collection
;; - Garbage Collection - GNU Emacs Lisp Reference Manual
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
(setq
 ;; message
 ;; garbage-collection-messages t
 )
;; increase thresholds
(cl-callf * gc-cons-threshold 2 2 2 2 2 2 2)
(cl-callf * gc-cons-percentage 2 2)

;;;; history
(cl-callf max history-length 1000)

;;;; key stroke
(setq echo-keystrokes 0.01)

;;;; cursor blinking
(blink-cursor-mode -1)

;;;; `yes-or-no-p'
;; simplify answering yes or no asking
(defalias 'yes-or-no-p #'y-or-n-p)

;;;; remove trailing whitespaces before saving file
(defun my-delete-trailing-whitespaces-before-saving ()
  (unless (memq major-mode '(markdown-mode))
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'my-delete-trailing-whitespaces-before-saving)

;;;; vc-mode
;; disable vc-mode
(setq vc-handled-backends nil)

;;;; indentation
(require 'cl-lib)

(setq-default tab-width 2)
(setq tab-stop-list (cl-loop for i from 2 to 120 by 2 collect i))

;; do not use tabs for indentation
(setq-default indent-tabs-mode nil)

(setq tab-always-indent t)

;;;; mode-line
;; do not show major-mode and minor-mode name
(setq-default mode-line-format
              (remq 'mode-line-modes (default-value 'mode-line-format)))

;; line number and column number
(line-number-mode 1)
(column-number-mode 1)

;;;; frame title
(setq frame-title-format "(　´・ω) (´・ω・) (・ω・｀) (ω・｀)")

;;;; `show-paren-mode'
(show-paren-mode 1)

;;;; display-fill-column-indicator-mode
(global-display-fill-column-indicator-mode 1)

;;;; mini-window
(setq  max-mini-window-height 9)

;;;; font

(my-with-package dash)
(require 'dash)

(defvar my-font-height 120)
(defvar my-font-fixed-pitch '("mononoki" "ubuntu mono"))
(defvar my-font-variable-pitch '("MigMix 2P"))
(defvar my-font-japanese '("Migu 2M"
                           "07YasashisaGothic"
                           "セプテンバーＭ-等幅教漢"))
(defvar my-font-symbol '("Noto Color Emoji"))
(defvar my-font-rescale-table
  '(("mononoki"
     ("Migu 2M"
      (96 . 1.125)
      (128 . 1.1500)
      (144 . 1.1000))
     ("07YasashisaGothic" (96 . 1.0625))
     ("セプテンバーＭ-等幅教漢" (96 . 1.0625)))))

(defun my-font--get-dpi ()
  (when (and (display-graphic-p)
             (executable-find "xrdb"))
    (save-match-data
      (-some--> (shell-command-to-string "xrdb -query -all")
        (when (string-match (rx bol "Xft.dpi:" (+ space) (group (+ num)) eol)
                            it)
          (match-string 1 it))
        (string-to-number it)))))

(when (display-graphic-p)
  (let ((fixed-pitch (-first #'font-info my-font-fixed-pitch))
        (variable-pitch (-first #'font-info my-font-variable-pitch))
        (japanese (-first #'font-info my-font-japanese))
        (symbol (-first #'font-info my-font-symbol)))
    (when fixed-pitch
      (set-face-attribute 'default nil
                          :family fixed-pitch
                          :height my-font-height
                          :weight 'normal)
      (set-face-attribute 'fixed-pitch nil
                          :family fixed-pitch
                          :height 'unspecified))
    (when variable-pitch
      (set-face-attribute 'variable-pitch nil
                          :family variable-pitch
                          :height 'unspecified))
    (when symbol
      (set-fontset-font "fontset-default"
                        'unicode
                        (font-spec :family symbol)
                        nil 'append))
    (when japanese
      (--each '(japanese-jisx0208 japanese-jisx0212 katakana-jisx0201)
        (set-fontset-font "fontset-default" it (font-spec :family japanese))))
    (setq face-font-rescale-alist
          (let ((scale-table
                 (alist-get fixed-pitch my-font-rescale-table nil nil #'equal))
                (dpi (or (my-font--get-dpi) 96)))
            (--map (let* ((scales (cdr it))
                          (default (alist-get 96 scales nil nil #'=))
                          (scale (alist-get dpi scales default nil #'=)))
                     (cons (car it) scale))
                   scale-table)))))

;;;; key binding

(require 'my-util)
(require 'my-init-commands)
(require 'my-memo)

(my-define-key global-map
  "C-h"   (kbd "DEL")
  "C-S-h" #'backward-kill-word
  "C-z"   #'undo

  ;; Replace `list-buffers' to `ibuffer'.
  ;; I want to show the buffer list into the current window, not
  ;; showing in other window.  And I want to switch to one of the
  ;; listed buffers.
  "C-x C-b" #'ibuffer
  [remap shell-command] #'async-shell-command

  ;; convert backward word
  "M-u" #'my-convert-word-continuously-upcase-backward
  "M-c" #'my-convert-word-continuously-capitalize-backward
  "M-l" #'my-convert-word-continuously-downcase-backward

  ;; memo
  "<f4>" #'my-memo-transient)

(my-define-key isearch-mode-map
  ;; Remapping "C-h" to "DEL" does not work (ref emacs' bug#20466).
  "C-h" (lookup-key isearch-mode-map (kbd "DEL")))

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

;;; image

;; add webp as one of extensions of image file
(eval-when-compile
  (cl-assert (not (member "webp" image-file-name-extensions))
             "webp is already registered in `image-file-name-extensions'"))
(push "webp" image-file-name-extensions)

;;; recentf

;; to prevent reducing `recentf-list' before loading config.
(require 'recentf)
(setq recentf-max-saved-items nil)

;; config is in my-config-recentf.el
(require 'my-config-recentf)

(recentf-mode 1)

;;; eldoc

(setq eldoc-idle-delay 0.2
      eldoc-echo-area-use-multiline-p t)

;;; whitespace

(require 'whitespace)
(require 'my-config-whitespace)

(global-whitespace-mode 1)

;;; libraries

(my-with-package s)
(my-with-package f)
(my-with-package dash)

(require 's)
(require 'f)
(require 'dash)

;;; shell

(autoload 'my-new-shell "my-config-shell" nil t)

;;; winner-mode

(winner-mode 1)

;;; save-place

(require 'saveplace)
(setq save-place-limit 10000)
(save-place-mode 1)

;;; generic-x

(require 'generic-x)

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

(when (and (not (display-graphic-p)) (getenv "DISPLAY"))
  (setq interprogram-cut-function #'my-xclip-cut
        interprogram-paste-function #'my-xclip-paste))

;;; company

(require 'company)
(require 'my-config-company)

(global-company-mode 1)

;;; company-prescient

(my-with-package company-prescient)
(require 'company-prescient)

(company-prescient-mode 1)

;;; eyebrowse

(my-with-package eyebrowse)

(require 'eyebrowse)
(require 'my-config-eyebrowse)

(eyebrowse-mode 1)

;;; iflipb

(my-with-package iflipb)

(my-define-key global-map
  "M-t" #'iflipb-next-buffer
  "M-T" #'iflipb-previous-buffer)

;;; ivy

(my-with-package ivy)

(require 'ivy)
(require 'my-config-ivy)

(ivy-mode 1)

;;; ivy-prescient

(my-with-package ivy-prescient)

;; NOTE: load after ivy and my-config-ivy
(require 'ivy-prescient)
(require 'my-config-ivy-prescient)

(ivy-prescient-mode 1)

;;; counsel

(my-with-package counsel)

(my-define-key global-map
  [remap yank-pop] #'counsel-yank-pop   ; M-y
  [remap execute-extended-command] #'counsel-M-x
  )

;;; js2-mode and tern

(my-with-package js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; mozc

(my-with-package mozc)

(my-define-key global-map
  ;; disable `toggle-input-method'
  "C-\\" nil
  ;; prevent calling unwanted commands by mistyping
  "M-m" nil
  "C-M-m" nil)

(require 'mozc)

(setq default-input-method "japanese-mozc")

;;; evil

;; Do not load evil before my-init-evil
(require 'my-init-evil)

;;; feebleline

(my-with-package feebleline)

(require 'feebleline)
(require 'my-config-feebleline)

(feebleline-mode 1)

;;; projectile

(my-with-package projectile)

(projectile-mode 1)

;;; which-key

(my-with-package which-key)

(which-key-mode 1)

;;; highlight-symbol

(my-with-package highlight-symbol)
(require 'my-util)

(my-define-key global-map
  "<f8>" #'highlight-symbol
  "<f9>" #'highlight-symbol-prev
  "<f10>" #'highlight-symbol-next)

;;; rjsx-mode

(my-with-package rjsx-mode)

(add-to-list 'auto-mode-alist
             (cons (rx (or "components" "pages") "/" (* any) ".js" eos)
                   #'rjsx-mode))

;;; yasnippet

(my-with-package yasnippet)
(require 'yasnippet)

;;;; snippet dir
(require 'my-init-path)

(setq yas-snippet-dirs
      (--> (list (my-path-dot-emacs-prj "yasnippet" "snippets"))
           (-filter #'file-exists-p it)))

;;;; global snippet
(defun my-yasnippet-enable-global-snippet ()
  (yas-activate-extra-mode 'fundamental-mode))

(add-hook 'yas-minor-mode-hook #'my-yasnippet-enable-global-snippet)

;;;; turn on minor mode
(yas-global-mode 1)

;;; markdown-mode

(my-with-package markdown-mode)

;;; json-mode

(my-with-package json-mode)

;;; gif-screencast

(my-with-package gif-screencast)

;;; yaml-mode

(my-with-package yaml-mode)

;;; lsp-mode

(my-with-package lsp-mode)

;;; `my-always-recenter-mode'

(require 'my-always-recenter)

(my-global-always-recenter-mode 1)

;;; `hl-line-mode'

(global-hl-line-mode 1)


(provide 'my-init-configuration)
;;; my-init-configuration.el ends here
