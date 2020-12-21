;;; my-config-help.el --- help-mode                     -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'help-mode)

;; Always select help window when opening it.
(setq help-window-select t)

;; evil
(my-with-package evil)
(require 'my-init-evil)
(require 'evil)

(evil-define-key 'motion help-mode-map
  ;; Use some keys for default commands not evil commands.
  (kbd "TAB") (lookup-key help-mode-map (kbd "TAB"))
  (kbd "q")   (lookup-key help-mode-map (kbd "q")))


(provide 'my-config-help-mode)
;;; my-config-help-mode.el ends here
