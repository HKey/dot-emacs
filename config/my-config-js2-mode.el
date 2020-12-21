;;; my-config-js2-mode.el --- js2-mode                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package js2-mode)

(require 'js2-mode)

;;;; config

;; indent width
(setq js2-basic-offset tab-width)

;; warn
(setq js2-strict-missing-semi-warning nil)

;; use `electric-pair-mode'
(add-hook 'js2-mode-hook #'electric-pair-local-mode)

;;;; tide

(my-with-package tide)

(add-hook 'js2-mode-hook #'tide-setup)


(provide 'my-config-js2-mode)
;;; my-config-js2-mode.el ends here
