;;; config-magit.el --- magit                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package magit)

(require 'magit)

;; Do not split window when opening magit.
(setq magit-display-buffer-function
      #'magit-display-buffer-same-window-except-diff-v1)


(provide 'config-magit)
;;; config-magit.el ends here
