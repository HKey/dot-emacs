;;; config-org-clock.el --- org-clock                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-clock)

;;;; Confirm when exit emacs without clock-out

(defun my-org-clock-confirm-clocking ()
  "Confirm to exit emacs without clock-out."
  (if org-clock-current-task
      (yes-or-no-p "org-clock is running, dou you want to kill Emacs?")
    t))

(add-to-list 'kill-emacs-query-functions #'my-org-clock-confirm-clocking)

;;;; Appearance

(setq org-clock-clocked-in-display 'frame-title)


(provide 'config-org-clock)
;;; config-org-clock.el ends here
