;;; init-alert.el --- alert.el                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package alert)

(require 'alert)

;; use notify-send if possible
(when (executable-find "notify-send")
  (setq alert-default-style 'libnotify))

;;;; Alerts

(defun my-alert-buffer-saving ()
  (alert "バッファを保存しました。"
         :title (buffer-name (current-buffer))))

(defun my-alert-emacs-startup ()
  (alert (format "%.3f秒掛かりました。"
                 (float-time (time-subtract after-init-time
                                            before-init-time)))
         :title "Emacsを起動しました"))

(defun my-alert-emacs-shutdown ()
  (alert (emacs-uptime)
         :title "Emacsを終了します"))

(add-hook 'after-save-hook #'my-alert-buffer-saving)
(add-hook 'after-init-hook #'my-alert-emacs-startup)
(add-hook 'kill-emacs-hook #'my-alert-emacs-shutdown)

(provide 'init-alert)
;;; init-alert.el ends here
