;;; config-org.el --- org                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'org)

;;; basic configuration

(setq
 ;; do not use indentation
 org-adapt-indentation nil

 ;; block if childern not done
 org-enforce-todo-dependencies t
 ;; org-enforce-todo-checkbox-dependencies t

 ;; also cycle archived tree
 ;; org-cycle-open-archived-trees t

 ;; simplify leading stars
 org-hide-leading-stars (display-graphic-p)

 ;; fontify quote block
 org-fontify-quote-and-verse-blocks t

 ;; do not fold as default
 org-startup-folded nil

 ;; open babel's window in the current window
 org-src-window-setup 'current-window

 ;; use font-lock of language's major-mode
 org-src-fontify-natively t

 ;; refile
 ;; only files are targets, not trees
 org-refile-targets '((org-agenda-files :level . 0) (nil :level . 0))
 org-refile-use-outline-path 'file
 org-refile-use-cache t
 )

;;;; key binding

(require 'lib-util)

;; narrowing
(my-define-key org-mode-map [remap narrow-to-defun] #'org-narrow-to-subtree)

;;;; speed commands

(my-with-package org-taskforecast)

(defun my-org-toggle-archive-tag ()
  "Do `org-toggle-archive-tag' and align tags.
This command only works for the current headline."
  (save-excursion
    (org-toggle-archive-tag)
    ;; just align tags because `org-toggle-archive-tag' does not align them
    (org-set-tags (org-get-tags nil t))))

(defun my-org-set-property-maybe (property value)
  "Set PROPERTY's value to VALUE unless it is not set yet."
  (unless (org-entry-get (point) property)
    (org-set-property property value)))

(defun my-org-set-created-property ()
  "Set CREATED property."
  (interactive)
  (let ((property "CREATED")
        (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (my-org-set-property-maybe property timestamp)))

(defun my-org-speed-move-safe-recenter (cmd)
  "Move cursor like `org-speed-move-safe' and recenter.
CMD is a movig command."
  (org-speed-move-safe cmd)
  (recenter 0))

(setq
 ;; enable speed commands
 org-use-speed-commands t

 ;; additional key bindings
 org-speed-commands-user
 '(("A" . my-org-toggle-archive-tag)
   ("S" . org-store-link)
   ("z" . org-add-note)
   ;; "C" overrides `org-shifttab'
   ("C" . my-org-set-created-property)
   ("x" . org-archive-to-archive-sibling)
   ("c" . org-force-cycle-archived)
   ;; "o": `org-open-at-point' with `save-excursion'
   ("o" . (progn
            (org-open-at-point)
            (save-excursion
              (evil-normal-state))))
   ("T" . my-org-touch)
   ("g" . org-taskforecast-register-task)
   ("G" . (org-refile t))
   ("E" . (org-entry-put nil "ORDERED" "t"))
   ;; move with recenter
   ("Outline Navigation (with recenter)")
   ;; ("n" . (my-org-speed-move-safe-recenter 'org-next-visible-heading))
   ;; ("p" . (my-org-speed-move-safe-recenter 'org-previous-visible-heading))
   ("f" . (my-org-speed-move-safe-recenter 'org-forward-heading-same-level))
   ("b" . (my-org-speed-move-safe-recenter 'org-backward-heading-same-level))
   ;; ("u" . (my-org-speed-move-safe-recenter 'outline-up-heading))
   ))

;;;; my-org-touch

(my-with-package dash)
(require 'dash)

(defvar my-org-atime-property "MY_ATIME")

(defun my-org-sort-entries-by-recency ()
  (interactive)
  (save-excursion
    (org-sort-entries nil ?R nil nil "CREATED"))
  (save-excursion
    (org-sort-entries nil ?R nil nil my-org-atime-property))
  (save-excursion
    (org-show-all)))

(define-minor-mode my-org-touch-mode
  "Automatically set MY_ATIME property.

  To enable this mode in a specific directory, put below into
  .dir-locals.el in the specific directory.

  ((org-mode . ((eval . (my-org-touch-mode 1)))))"
  :global nil
  (let ((hooks '(org-clock-in-hook
                 org-clock-out-hook
                 org-after-tags-change-hook
                 org-after-todo-state-change-hook
                 org-after-refile-insert-hook
                 org-checkbox-statistics-hook)))
    (if my-org-touch-mode
        (progn
          (--each hooks
            (add-hook it #'my-org-touch nil t)))
      (--each hooks
        (remove-hook it #'my-org-touch t)))))

(defun my-org-touch ()
  "Set or update MY_ATIME to current timestamp."
  (interactive)
  (let ((touch
         (lambda ()
           (org-set-property
            my-org-atime-property
            (format-time-string "[%Y-%m-%d %a %H:%M]")))))
    (save-excursion
      (save-restriction
        (widen)
        (funcall touch)
        (while (> (funcall outline-level) 1)
          (outline-up-heading 1 t)
          (funcall touch))))))

;;;; company

(my-with-package company)
(require 'company)

(defun my-org-setup-company ()
  (setq-local company-backends '((company-yasnippet company-files))))

(add-hook 'org-mode-hook #'my-org-setup-company)

;;;; org-set-tags-command

(my-with-package dash)
(my-with-package s)
(require 'dash)
(require 's)

(defun my-org-get-buffer-tags ()
  "Get a table of all tags used in the buffer, for completion."
  (org-with-point-at (point-min)
    (save-match-data
      (--> (cl-loop while (re-search-forward org-tag-line-re nil t)
                    for tags = (split-string (match-string-no-properties 2)
                                             ":")
                    append tags
                    collect (s-join ":" (-sort #'string-lessp tags)))
           (append it
                   (--> (-map #'car org-current-tag-alist)
                        (-filter #'stringp it)))
           (append org-file-tags it)
           (delete-dups it)))))

(defun my-org-get-buffers-tags ()
  "Get org buffers tags table."
  (--> (buffer-list)
       (--filter (buffer-local-value 'major-mode it) it)
       (--map (with-current-buffer it
                (my-org-get-buffer-tags))
              it)
       (apply #'append it)
       (delete-dups it)))

(defun my-org-set-tags ()
  (declare (interactive-only t))
  (interactive)
  (--> (completing-read
        "Tags: "
        (my-org-get-buffers-tags)
        nil
        nil
        (-some--> (org-get-tags nil t)
          (s-join ":" it)
          (concat ":" it ":")))
       ;; regexp from `org-set-tags-command'
       (replace-regexp-in-string "[^[:alnum:]_@#%]+" ":" it)
       (save-excursion
         (org-back-to-heading)
         (org-set-tags it))))

(defun my-org-set-tags-advice (fn &rest args)
  "Advice to override `org-set-tags-command' with `my-org-set-tags'"
  (if (called-interactively-p 'any)
      (call-interactively #'my-org-set-tags)
    (warn (concat "`org-set-tags-command' has been called as not interactive "
                  "(`my-org-set-tags-advice')"))
    (apply fn args)))

(advice-add 'org-set-tags-command :around #'my-org-set-tags-advice)


(provide 'config-org)
;;; config-org.el ends here
