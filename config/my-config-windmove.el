;;; my-config-windmove.el --- windmove                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'windmove)

;; Split window if there is no split window.
(defun my-windmove-split-as-needed (original dir &optional arg window)
  (let ((result (funcall original dir arg window)))
    (or (unless (window-minibuffer-p result)
          result)
        (split-window window
                      nil
                      (cond ((eq dir 'up) 'above)
                            ((eq dir 'down) 'below)
                            (dir))))))

(advice-add #'windmove-find-other-window
            :around
            #'my-windmove-split-as-needed)

;; Enable wrapping around
(setq windmove-wrap-around t)


(provide 'my-config-windmove)
;;; my-config-windmove.el ends here
