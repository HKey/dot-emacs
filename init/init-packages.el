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


;;; dimmer

(use-package dimmer
  :init
  (dimmer-mode 1)
  :config
  (setq dimmer-exclusion-regexp "helm"  ; Disable dimmer for helm buffers
        dimmer-fraction 0.4))



(provide 'init-packages)
;;; init-packages.el ends here
