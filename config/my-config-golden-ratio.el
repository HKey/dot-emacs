;;; my-config-golden-ratio.el --- golden-ratio          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package golden-ratio)
(require 'golden-ratio)

(cl-callf append golden-ratio-extra-commands
  (list 'evil-window-up
        'evil-window-down
        'evil-window-left
        'evil-window-right
        'evil-window-next
        'evil-window-prev
        'evil-window-mru
        'evil-window-top-left
        'evil-window-bottom-right))
(setq golden-ratio-max-width 80)


(provide 'my-config-golden-ratio)
;;; my-config-golden-ratio.el ends here
