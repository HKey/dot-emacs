;;; my-config-typescript-mode.el --- typescript-mode  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package typescript-mode)

(require 'typescript-mode)

;;; indent

(setq typescript-indent-level tab-width)

;;; electric-pair-mode

(add-hook 'typescript-mode-hook #'electric-pair-local-mode)

;;; tide

(my-with-package tide)

(add-hook 'typescript-mode-hook #'tide-setup)


(provide 'my-config-typescript-mode)
;;; my-config-typescript-mode.el ends here
