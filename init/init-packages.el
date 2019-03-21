;;; init-packages.el --- Package list and basic configuration  -*- lexical-binding: t; -*-

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
        use-package-always-ensure t))

;;; rust

(use-package rust-mode)

(use-package racer)

(use-package flycheck-rust)


;;; elisp

(use-package comment-or-uncomment-sexp)

(use-package lisp-extra-font-lock)

(use-package paredit)


;;; dimmer

(use-package dimmer
  :init
  (dimmer-mode 1)
  :config
  (setq dimmer-exclusion-regexp "helm"  ; Disable dimmer for helm buffers
        dimmer-fraction 0.4))


;;; eyebrowse

(use-package eyebrowse
  :functions evil-define-key
  :bind (:map eyebrowse-mode-map
              ("C-t n" . eyebrowse-next-window-config)
              ("C-t p" . eyebrowse-prev-window-config)
              ("C-t w" . eyebrowse-switch-to-window-config))
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-t"))
  (eyebrowse-mode 1)
  (with-eval-after-load 'evil
    ;; Disable default "C-t" in evil.
    (evil-define-key 'normal 'global eyebrowse-keymap-prefix nil))
  :config
  (setq eyebrowse-wrap-around t))


;;; iflipb

(use-package iflipb
  :bind (:map global-map
              ("M-t" . iflipb-next-buffer)
              ("M-T" . iflipb-previous-buffer)))


;;; themes

(use-package paper-theme)



(provide 'init-packages)
;;; init-packages.el ends here
