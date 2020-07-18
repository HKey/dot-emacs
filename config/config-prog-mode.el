;;; config-prog-mode.el --- prog-mode                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'prog-mode)

;;;; utility minor modes

;; eldoc
(add-hook 'prog-mode-hook #'eldoc-mode)

;; rainbow-delimiters
(my-with-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; highlight-parentheses
(my-with-package highlight-parentheses)
(add-hook 'prog-mode-hook #'highlight-parentheses-mode)

;; highlight-numbers
(my-with-package highlight-numbers)
(add-hook 'prog-mode-hook #'highlight-numbers-mode)

;;;; evil

(my-with-package evil)
(require 'evil)
(require 'config-evil)

;; Use symbol motions for E, B, W and gE.
(evil-define-key 'motion prog-mode-map
  [remap evil-forward-WORD-begin]  #'my-evil-forward-symbol-begin
  [remap evil-forward-WORD-end]    #'my-evil-forward-symbol-end
  [remap evil-backward-WORD-begin] #'my-evil-backward-symbol-begin
  [remap evil-backward-WORD-end]   #'my-evil-backward-symbol-end)


(provide 'config-prog-mode)
;;; config-prog-mode.el ends here
