;;; my-bootstrap.el --- bootstrap script             -*- lexical-binding: t; -*-

(require 'package)

;; add repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; package
(defun my--with-package-ensure-install (package)
  (unless (package-installed-p package)
    ;; Allow warnings when installing external packages.
    (let ((byte-compile-error-on-warn nil))
      (package-install package)))
  (add-to-list 'package-selected-packages package))

(defmacro my-with-package (package)
  (declare (indent 1))
  `(eval-and-compile
     (my--with-package-ensure-install ',package)))

;;; `load-prefer-newer'

;; Always load newer file to avoid loading old files in byte compiling.
(setq load-prefer-newer t)

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
