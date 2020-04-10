;;; my-bootstrap.el --- bootstrap script             -*- lexical-binding: t; -*-

;; use `eval-and-compile' to install use-package when compiling
(eval-and-compile
  (require 'package)

  ;; add repositories
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "`use-package' package is missing, install it?: ")
      (package-refresh-contents)
      (package-install 'use-package))))

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
