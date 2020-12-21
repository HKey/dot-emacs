;;; my-config-flycheck-posframe.el --- flycheck-posframe  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package flycheck-posframe)

(require 'flycheck-posframe)

(flycheck-posframe-configure-pretty-defaults)
(set-face-attribute 'flycheck-posframe-border-face nil :inherit 'default)
(setq flycheck-posframe-border-width 2)


(provide 'my-config-flycheck-posframe)
;;; my-config-flycheck-posframe.el ends here
