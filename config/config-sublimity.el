;;; config-sublimity.el --- sublimity                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package sublimity)

(require 'sublimity)

;;;; enable `auto-hscroll-mode'

;; `sublimity-mode' disables `auto-hscroll-mode', so I turn on it again
;; using sublimity's hook.

(defun my-sublimity-enable-auto-hscroll-mode ()
  (setq auto-hscroll-mode t))

(add-hook 'sublimity-mode-hook #'my-sublimity-enable-auto-hscroll-mode)


(provide 'config-sublimity)
;;; config-sublimity.el ends here
