;;; config-rust-mode.el --- Rust                -*- lexical-binding: t; -*-

(require 'rust-mode)
(require 'racer)

(defun my-rust-mode-hook ()
  (eldoc-mode 1)
  (racer-mode 1))

(add-hook 'rust-mode-hook #'my-rust-mode-hook)

(provide 'config-rust-mode)
;;; config-rust-mode.el ends here
