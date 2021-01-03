;;; my-always-recenter.el --- my-always-recenter-mode  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package dash)

(require 'dash)

(defvar my-always-recenter-ignore-commands
  (list #'recenter-top-bottom
        #'scroll-up-line
        #'scroll-down-line
        #'scroll-up-command
        #'scroll-down-command))

(defun my-always-recenter--recenter ()
  (unless (or (minibufferp)
              (memq this-command my-always-recenter-ignore-commands)
              (not (eq (window-buffer) (current-buffer))))
    (recenter)))

(defvar my-always-recenter--last-minibuffer-height nil)

(defun my-always-recenter--window-conf-change ()
  ;; Recenter when minibuffer height changed.  Mainly this is for
  ;; handling disappearing of iflipb's vertical buffer list.
  (let ((current-minibuffer-height
         (window-height (minibuffer-window))))
    (when (and my-always-recenter--last-minibuffer-height
               (/= my-always-recenter--last-minibuffer-height
                   current-minibuffer-height))
      (my-always-recenter--recenter))
    (setq my-always-recenter--last-minibuffer-height
          current-minibuffer-height)))

(define-minor-mode my-always-recenter-mode
  "Always recentering mode."
  :init-value nil
  (let ((hook-fns
         `((post-command-hook . ,#'my-always-recenter--recenter)
           (window-configuration-change-hook
            . ,#'my-always-recenter--window-conf-change))))
    (if my-always-recenter-mode
        (--each hook-fns
          (-let (((hook . fn) it))
            (add-hook hook fn 100 t)))
      (--each hook-fns
        (-let (((hook . fn) it))
          (remove-hook hook fn t))))))

(define-globalized-minor-mode my-global-always-recenter-mode
  my-always-recenter-mode (lambda () (my-always-recenter-mode 1))
  ;; `:group' is to suppress compiler warning "fails to specify
  ;; containing group", currently `emacs' group is set but it may not
  ;; be the proper group for this code.
  :group 'emacs)


(provide 'my-always-recenter)
;;; my-always-recenter.el ends here
