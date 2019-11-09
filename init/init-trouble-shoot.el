;;; init-trouble-shoot.el ---                         -*- lexical-binding: t; -*-

;;; Code:

;;;; mozc cursor jump trouble shooting

;; Sometimes the cursor jumps for the bottom of the current window.
;; It may be my mistake pressing M-r when I would call `mozc-temp-convert-dwim'
;; bound to M-n.
;; So I add a logging advice to `move-to-window-line-top-bottom' and `mozc-temp-convert-dwim'
;; to log that whether I pressed M-r.

;; Sometimes the cursor jumps for the bottom of the current window.
;; It may be caused by my mistake that is pressing M-r when I would call
;; `mozc-temp-convert-dwim' bound to M-n.
;; So I add a logging advice to `move-to-window-line-top-bottom' and
;; `mozc-temp-convert-dwim' to log that whether I pressed M-r.

(defun my-trouble-shoot-log-command-calling (&rest _)
  (message (concat "Comannd called:\n"
                   "  command: %s\n"
                   "  keys:    %s")
           this-command
           (format-kbd-macro (this-command-keys))))

(with-eval-after-load 'mozc-temp
  (advice-add #'move-to-window-line-top-bottom :before #'my-trouble-shoot-log-command-calling)
  (advice-add #'mozc-temp-convert-dwim :before #'my-trouble-shoot-log-command-calling))


(provide 'init-trouble-shoot)
;;; init-trouble-shoot.el ends here
