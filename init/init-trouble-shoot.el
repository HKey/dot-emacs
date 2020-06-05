;;; init-trouble-shoot.el ---                         -*- lexical-binding: t; -*-

;;; Code:

;;;; mozc cursor jump trouble shooting

;; Sometimes the cursor jumps for the bottom of the current window.
;; It may be my mistake pressing M-r when I would call `mozc-temp-convert-dwim'
;; bound to M-n.
;; So I add a logging function which records command name, key sequence
;; and time.

(require 'my-bootstrap)
(use-package dash)

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

;;;; suddenly default-directory changed

;; Sometimes buffer's `default-directory' is changed to not appropriate one.
(defvar my-trouble-shoot-default-directory (make-hash-table))

(defun my-trouble-shoot-default-directory-post-hook (&rest _)
  (cl-loop for buffer in (buffer-list)
           unless (gethash buffer my-trouble-shoot-default-directory)
           do (puthash buffer
                       (buffer-local-value 'default-directory buffer)
                       my-trouble-shoot-default-directory)
           unless (minibufferp buffer)
           unless (equal (buffer-local-value 'default-directory buffer)
                         (gethash buffer my-trouble-shoot-default-directory))
           do (warn (concat "`default-directory' has been changed\n"
                            "   command: %s\n"
                            "    buffer: %s\n"
                            "  recorded: %s\n"
                            "   current: %s")
                    this-command
                    buffer
                    (gethash buffer my-trouble-shoot-default-directory)
                    (buffer-local-value 'default-directory buffer))))

(add-hook 'post-command-hook #'my-trouble-shoot-default-directory-post-hook)

(provide 'init-trouble-shoot)
;;; init-trouble-shoot.el ends here
