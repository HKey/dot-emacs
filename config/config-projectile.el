;;; config-projectile.el --- projectile              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package projectile)

(require 'projectile)

;; use default to use ivy instead of ido
(setq projectile-completion-system 'default)


(provide 'config-projectile)
;;; config-projectile.el ends here
