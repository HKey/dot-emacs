;;; my-config-org-refile.el --- org-refile           -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org-refile)

(setq
 ;; only files are targets, not trees
 org-refile-targets '((org-agenda-files :level . 0) (nil :level . 0))

 org-refile-use-outline-path 'file
 org-refile-use-cache t)


(provide 'my-config-org-refile)
;;; my-config-org-refile.el ends here
