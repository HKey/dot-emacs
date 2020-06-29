;;; config-org-capture.el --- org-capture            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-capture)


;;;; `org-capture-templates'

(my-with-package f)
(require 'f)
(require 'init-path)

(defun my-org-capture-memo-path (dir)
  "Create memo file path based on DIR"
  (--> (format-time-string "%Y/%m/%Y-%m-%d-%H%M%S.org" (current-time))
       (f-join dir it)
       (prog1 it (make-directory (file-name-directory it) t))))

(setq org-capture-templates
      `(("m" "Memo" entry
         (file ,(lambda () (my-org-capture-memo-path (my-path-org-memo))))
         (file ,(my-path-org-capture-templates "basic.org"))
         :kill-buffer t)
        ("i" "Inbox" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox.org"))
         :empty-lines 1)
        ("c" "Inbox from clipboard" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox-clipboard.org"))
         :empty-lines 1)))


(provide 'config-org-capture)
;;; config-org-capture.el ends here
