;;; my-config-ielm.el --- ielm                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'ielm)

;;; key binding

(require 'my-util)

(my-define-key ielm-map
  "C-j" #'newline-and-indent)

;;; paredit

(my-with-package paredit)

(add-hook 'ielm-mode-hook #'enable-paredit-mode)


(provide 'my-config-ielm)
;;; my-config-ielm.el ends here
