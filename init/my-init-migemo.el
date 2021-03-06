;;; my-init-migemo.el --- migemo                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package migemo)

(require 'migemo)

;;;; configuration

(setq migemo-command (executable-find "cmigemo")
      migemo-dictionary (expand-file-name "~/migemo/dict/utf-8.d/migemo-dict")
      ;; migemo-coding-system 'utf-8
      migemo-use-default-isearch-keybinding nil
      migemo-user-dictionary nil
      migemo-regex-dictionary nil
      migemo-use-frequent-pattern-alist t
      migemo-use-pattern-alist t)

(unless migemo-command
  (warn "cmigemo command not found"))
(unless (file-exists-p migemo-dictionary)
  (warn "migemo dictionary not found"))

;;;; initialization

(migemo-init)


(provide 'my-init-migemo)
;;; my-init-migemo.el ends here
