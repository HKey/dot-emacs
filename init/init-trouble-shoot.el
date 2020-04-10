;;; init-trouble-shoot.el ---                         -*- lexical-binding: t; -*-

;;; Code:

;;;; mozc cursor jump trouble shooting

;; Sometimes the cursor jumps for the bottom of the current window.
;; It may be my mistake pressing M-r when I would call `mozc-temp-convert-dwim'
;; bound to M-n.
;; So I add a logging function which records command name, key sequence
;; and time.

(require 'my-bootstrap)
(require 'dash)

(defvar my-trouble-shoot-log-command-record nil)

(defun my-trouble-shoot-log-command-calling ()
  (push `(:command ,this-command
                   :key ,(format-kbd-macro (this-command-keys))
                   :time ,(current-time))
        my-trouble-shoot-log-command-record))

(defun my-trouble-shoot-show-command-log ()
  (interactive)
  (with-current-buffer (get-buffer-create " *my-trouble-shoot-command-log*")
    (erase-buffer)
    (--each (reverse my-trouble-shoot-log-command-record)
      (-let (((&plist :command command :key key :time time) it))
        (insert "\n"
                "time: " (format-time-string "%Y-%m-%d %H:%M:%S" time) "\n"
                " cmd: " (cond ((symbolp command) (symbol-name command))
                               ((stringp command) (format-kbd-macro command))
                               (t (format "%S" command))) "\n"
                " key: " key "\n")))
    (switch-to-buffer (current-buffer))))

;; (add-hook 'post-command-hook #'my-trouble-shoot-log-command-calling)
(add-hook 'pre-command-hook #'my-trouble-shoot-log-command-calling)


(provide 'init-trouble-shoot)
;;; init-trouble-shoot.el ends here
