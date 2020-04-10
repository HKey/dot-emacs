;;; init.el ---   -*- lexical-binding: t; -*-

(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name))))
  (add-to-list 'load-path (expand-file-name "bootstrap" dot-emacs-root)))

(require 'my-bootstrap)
(require 'use-package)

(use-package el-init)


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
