;;; init.el ---   -*- lexical-binding: t; -*-

(require 'package)

;; add repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'el-init)
  (when (yes-or-no-p "`el-init' package is missing, install it?: ")
    (package-refresh-contents)
    (package-install 'el-init)))

(require 'el-init)


(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name))))
  (el-init-load dot-emacs-root
                :subdirectories '(("init" t)
                                  ("config" t))
                :wrappers '(el-init-require/lazy
                            el-init-require/record-error)))

;;; init.el ends here
