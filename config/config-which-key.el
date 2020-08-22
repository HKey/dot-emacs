;;; config-which-key.el --- which-key                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package which-key)

(require 'which-key)

(setq which-key-max-description-length nil
      which-key-side-window-max-height 0.5)


(provide 'config-which-key)
;;; config-which-key.el ends here
