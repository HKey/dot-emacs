;;; init.el ---   -*- lexical-binding: t; -*-

;; gccemacs (native comp)
(custom-set-variables
 '(comp-speed 3))

(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name))))
  (add-to-list 'load-path (expand-file-name "bootstrap" dot-emacs-root)))

(require 'my-bootstrap)

(my-with-package el-init)

;;; el-init loading

(require 'el-init)

(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name)))
      (el-init-lazy-init-regexp "\\`my-config-\\(.+\\)\\'"))
  (add-to-list 'load-path (expand-file-name "lib" dot-emacs-root))
  (el-init-load dot-emacs-root
                :subdirectories '(("init" t)
                                  ("config" t))
                :wrappers '(el-init-require/lazy)))

;; load private configuration from "~/.emacs.d/private-conf"
(let ((el-init-lazy-init-regexp "\\`my-config-private-\\(.+\\)\\'"))
  (el-init-load (expand-file-name "private-conf" user-emacs-directory)
                :wrappers '(el-init-require/lazy)))

;;; init.el ends here
