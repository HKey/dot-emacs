;;; my-config-org-capture.el --- org-capture            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-capture)


;;;; `org-capture-templates'

(require 'my-init-path)

(setq org-capture-templates
      `(("i" "Inbox" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox.org"))
         :empty-lines 1)
        ("c" "Inbox from clipboard" entry
         (file+datetree ,(lambda () (my-path-org-agenda "inbox.org")))
         (file ,(my-path-org-capture-templates "inbox-clipboard.org"))
         :empty-lines 1)
        ("d" "Datetree in this buffer" entry
         (file+datetree
          ,(lambda ()
             (unless (eq major-mode 'org-mode)
               (user-error "This capturing can be called on an org-mode buffer"))
             (unless (buffer-file-name)
               (user-error "This capturing can be called on a file buffer"))
             (buffer-file-name)))
         (file ,(my-path-org-capture-templates "basic.org"))
         :empty-lines 1)))


(provide 'my-config-org-capture)
;;; my-config-org-capture.el ends here
