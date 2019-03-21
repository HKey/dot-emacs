;;; init-my-hide-mode-line.el --- Hide/Show mode-line -*- lexical-binding: t; -*-

;;; Definition

(defvar-local my-hide-mode-line-format nil
  "Hidden `mode-line-format'.")

(defun my-hide-mode-line-hide (&rest _)
  "Hide mode-line."
  (interactive)
  (when mode-line-format
    (setq my-hide-mode-line-format mode-line-format
          mode-line-format nil)
    (force-mode-line-update)))

(defun my-hide-mode-line-show (&rest _)
  "Show mode-line."
  (interactive)
  (unless mode-line-format
    (setq mode-line-format my-hide-mode-line-format
          my-hide-mode-line-format nil)
    (force-mode-line-update)))

(defun my-hide-mode-line-toggle (&rest _)
  "Toggle hide/show mode-line."
  (interactive)
  (if mode-line-format
      (my-hide-mode-line-hide)
    (my-hide-mode-line-show)))


;;; Configuration

(define-key global-map (kbd "<f12>") #'my-hide-mode-line-toggle)



(provide 'init-my-hide-mode-line)
;;; init-my-hide-mode-line.el ends here
