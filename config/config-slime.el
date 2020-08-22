;;; config-slime.el --- slime                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package slime)
(my-with-package dash)
(my-with-package paredit)

(require 'slime)
(require 'dash)

(setq inferior-lisp-program "ccl"
      slime-lisp-implementations '((clozurecl ("ccl"))
                                   (sbcl      ("sbcl"))
                                   (clisp     ("clisp"))
                                   (ecl       ("ecl")))
      slime-net-coding-system 'utf-8-unix)
(--each '(slime-mode-hook slime-repl-mode-hook)
  (add-hook it #'enable-paredit-mode))


(provide 'config-slime)
;;; config-slime.el ends here
