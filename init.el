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

(defun my-el-init-require/dont-load-lib (feature &optional filename noerror)
  "Do not load lib-*.el"
  (when (or el-init-overridden-require-p
            (not (string-match-p "\\`lib-" (symbol-name feature))))
    (el-init-next feature filename noerror)))

(let ((dot-emacs-root (file-name-directory
                       (or (buffer-file-name) load-file-name)))
      (el-init-lazy-init-regexp "\\`config-\\(.+\\)\\'"))
  (el-init-load dot-emacs-root
                :subdirectories '(("init" t)
                                  ("config" t)
                                  ("util" t))
                :wrappers '(my-el-init-require/dont-load-lib
                            el-init-require/lazy
                            ;; el-init-require/record-error
                            )))

;; load private configuration from "~/.emacs.d/private-conf"
(let ((el-init-lazy-init-regexp "\\`config-private-\\(.+\\)\\'"))
  (el-init-load (expand-file-name "private-conf" user-emacs-directory)
                :wrappers '(el-init-require/lazy)))

;;; init.el ends here
