;;; init.el ---   -*- lexical-binding: t; -*-

;;; Package setup
(require 'package)

;; add repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(unless (package-installed-p 'el-init)
  (when (yes-or-no-p "`el-init' package is missing, install it?: ")
    (package-refresh-contents)
    (package-install 'el-init)))


;;; el-init loading

(require 'el-init)

(setq el-init-lazy-init-regexp "\\`config-\\(.+\\)\\'")

(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name))))
  (el-init-load dot-emacs-root
                :subdirectories '(("init" t)
                                  ("config" t))
                :wrappers '(el-init-require/lazy
                            ;; el-init-require/record-error
                            )))

;; load private configuration from "~/.emacs.d/private-conf"
(el-init-load (expand-file-name "private-conf" user-emacs-directory)
              :wrappers '(el-init-require/lazy))

;;; init.el ends here
