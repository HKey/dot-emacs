;;; init-exec-path-from-shell.el --- exec-path-from-shell  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package exec-path-from-shell)

(require 'exec-path-from-shell)

;; I'm using .zshrc to setup environment variables.
(setq exec-path-from-shell-check-startup-files nil)

;; Additional environment variables
(setq exec-path-from-shell-variables
      `(,@exec-path-from-shell-variables
        "DICTIONARY"                    ; hunspell
        "GOPATH"                        ; golang
        "PYTHONUSERBASE"                ; pip (local)
        "PIPENV_VENV_IN_PROJECT"        ; pipenv
        ))

(exec-path-from-shell-initialize)


(provide 'init-exec-path-from-shell)
;;; init-exec-path-from-shell.el ends here
