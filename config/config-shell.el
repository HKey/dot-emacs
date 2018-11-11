;;; config-shell.el --- M-x shell                -*- lexical-binding: t; -*-

(require 'shell)
(require 'comint)


(defun my-shell-hook ()
  ;; disable echoing when input commands
  (setq-local comint-process-echoes t))

(add-hook 'shell-mode-hook #'my-shell-hook)

(provide 'config-shell)
;;; config-shell.el ends here
