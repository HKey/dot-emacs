;;; config-shell.el --- M-x shell                -*- lexical-binding: t; -*-

(require 'shell)
(require 'comint)
(require 'company)


(defun my-shell-hook ()
  ;; disable echoing when input commands
  (cl-assert (null comint-process-echoes))
  (setq-local comint-process-echoes t)
  ;; disable company auto complete
  (setq-local company-minimum-prefix-length nil))

(add-hook 'shell-mode-hook #'my-shell-hook)


(defun my-new-shell ()
  "Make new shell"
  (interactive)
  (shell (format "*shell from %s*" (buffer-name))))

(provide 'config-shell)
;;; config-shell.el ends here
