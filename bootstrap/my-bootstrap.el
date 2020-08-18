;;; my-bootstrap.el --- bootstrap script             -*- lexical-binding: t; -*-

;; use `eval-and-compile' to install use-package when compiling
(eval-and-compile
  (require 'package)

  ;; add repositories
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  ;; pinning
  (setq package-pinned-packages
        '((org-taskforecast . "manual")
          (pocket-reader . "manual")))

  (package-initialize))

;; package
(defvar my--with-package-refreshed-p nil)
(defun my--with-package-ensure-install (package)
  (unless (package-installed-p package)
    (unless my--with-package-refreshed-p
      (package-refresh-contents)
      (setq my--with-package-refreshed-p t))
    (package-install package))
  (add-to-list 'package-selected-packages package))

(defmacro my-with-package (package)
  (declare (indent 1))
  `(eval-and-compile
     (my--with-package-ensure-install ',package)))

;; use-package
(eval-and-compile
  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "`use-package' package is missing, install it?: ")
      (package-refresh-contents)
      (package-install 'use-package))))

;;; use-package

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

;;; `load-prefer-newer'

;; Always load newer file to avoid loading old files in byte compiling.
(setq load-prefer-newer t)

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
