;;; my-always-recenter.el --- my-always-recenter-mode  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package dash)

(require 'dash)

(defvar my-always-recenter-ignore-commands
  (list #'recenter-top-bottom
        #'scroll-up-line
        #'scroll-down-line
        #'scroll-up-command
        #'scroll-down-command
        #'iflipb-next-buffer
        #'iflipb-previous-buffer))

(defun my-always-recenter--recenter ()
  (unless (or (minibufferp)
              (memq this-command my-always-recenter-ignore-commands)
              (not (eq (window-buffer) (current-buffer))))
    (recenter)))

(define-minor-mode my-always-recenter-mode
  "Always recentering mode."
  :init-value nil
  (let ((hooks '(post-command-hook
                 window-configuration-change-hook)))
    (if my-always-recenter-mode
        (--each hooks
          (add-hook it #'my-always-recenter--recenter 100 t))
      (--each hooks
        (remove-hook it #'my-always-recenter--recenter t)))))

(define-globalized-minor-mode my-global-always-recenter-mode
  my-always-recenter-mode (lambda () (my-always-recenter-mode 1))
  ;; `:group' is to suppress compiler warning "fails to specify
  ;; containing group", currently `emacs' group is set but it may not
  ;; be the proper group for this code.
  :group 'emacs)


(provide 'my-always-recenter)
;;; my-always-recenter.el ends here
