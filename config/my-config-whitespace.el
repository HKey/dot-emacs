;;; my-config-whitespace.el --- whitespace           -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'whitespace)

;; config
(setq whitespace-display-mappings
      '((space-mark   ?　 [?□])
        ;; (newline-mark ?\n [?↩ ?\n])
        ;; (tab-mark     ?\t [?^ ?\t])
        ))

;; See `whitespace-style-value-list'
(setq whitespace-style
      '(face
        tabs
        ;; spaces
        trailing
        ;; lines
        ;; lines-tail
        ;; newline
        ;; empty
        ;; indentation
        ;; indentation::tab
        ;; indentation::space
        ;; space-after-tab
        ;; space-after-tab::tab
        ;; space-after-tab::space
        ;; space-before-tab
        ;; space-before-tab::tab
        ;; space-before-tab::space
        ;; help-newline
        tab-mark
        space-mark
        ;; newline-mark
        ))

;; ignore some major-mode
(setq whitespace-global-modes
      '(not magit-log-mode))


(provide 'my-config-whitespace)
;;; my-config-whitespace.el ends here
