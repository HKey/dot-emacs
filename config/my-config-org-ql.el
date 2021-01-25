;;; my-config-org-ql.el --- org-ql                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(my-with-package org-ql)
(require 'org-ql)

;;; Show result in the same window

(require 'org-ql-view)

(setq org-ql-view-display-buffer-action
      (cons #'display-buffer-same-window nil))

;;; my commands

(my-with-package transient)
(require 'transient)

(defconst my-org-ql--query-next-actions
  '(and (todo)
        (not (org-entry-blocked-p))
        (not (or (scheduled) (deadline)))))

(defun my-org-ql-next-actions ()
  "Search next actions in this buffer."
  (interactive)
  (org-ql-search (current-buffer)
    my-org-ql--query-next-actions))

(defun my-org-ql-next-actions-sparse-tree ()
  "Show sparse tree of next actions in this buffer."
  (interactive)
  (org-ql-sparse-tree
   my-org-ql--query-next-actions))

(defun my-org-ql-completed-project ()
  "Search completed but todo projects in this buffer."
  (interactive)
  (org-ql-search
    (org-agenda-files)
    '(and (todo)
          (not (descendants (todo)))
          (descendants (done)))))

(defun my-org-ql-recent-items ()
  "Search recent items in this buffer."
  (interactive)
  (org-ql-view-recent-items :num-days 3 :files (current-buffer)))

(transient-define-prefix my-org-ql-transient ()
  [["Query for this buffer"
    ("r" "recent items" my-org-ql-recent-items)
    ("n" "next actions" my-org-ql-next-actions)
    ("P" "completed projects" my-org-ql-completed-project)]
   ["Sparse tree for this buffer"
    ("sn" "next actions sparse tree" my-org-ql-next-actions-sparse-tree)
    ("sq" "show all (exit sparse tree)" org-show-all)]]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])


(provide 'my-config-org-ql)
;;; my-config-org-ql.el ends here
