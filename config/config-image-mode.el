;;; config-image-mode.el --- image-mode              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'image-mode)

;;;; miscs

;; Resize images immediately.
(setq image-auto-resize t
      image-auto-resize-on-window-resize 0.01)

;;;; command

(defun my-image-delete-current-image ()
  "Delete current image file and move to the next image."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (when (yes-or-no-p (format "Delete this image (%s)?" file))
      (image-next-file)
      (delete-file file))))

;;;; key binding

(require 'lib-util)

(my-define-key image-mode-map (kbd "d") #'my-image-delete-current-image)

;;;; make cursor invisible

(my-with-package dash)
(require 'init-evil)
(require 'evil)
(require 'dash)

(defun my-image-invisible-cursor ()
  "Make cursor invisible"
  (setq-local cursor-type nil)
  ;; evil
  (--each (-keep #'cdr (evil-state-property t :cursor))
    (set (make-local-variable it) '(nil))))

(add-hook 'image-mode-hook #'my-image-invisible-cursor)


(provide 'config-image-mode)
;;; config-image-mode.el ends here
