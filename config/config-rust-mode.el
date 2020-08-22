;;; config-rust-mode.el --- Rust                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package rust-mode)
(my-with-package racer)
(my-with-package flycheck)
(my-with-package flycheck-rust)

(require 'rust-mode)
(require 'racer)
(require 'flycheck)
(require 'flycheck-rust)

(defun my-rust-mode-hook ()
  (eldoc-mode 1)
  (racer-mode 1)
  (flycheck-rust-setup)
  (flycheck-mode 1))

(add-hook 'rust-mode-hook #'my-rust-mode-hook)

(provide 'config-rust-mode)
;;; config-rust-mode.el ends here
