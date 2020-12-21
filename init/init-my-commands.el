;;; init-my-commands.el --- My utility commands   -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package s)
(my-with-package f)
(my-with-package dash)

(require 's)
(require 'f)
(require 'dash)


(defun my-open-gentoo-ebuild (pkg)
  "Open ebuild file with equery."
  (interactive "sPackage: ")
  (--> (shell-command-to-string (concat "equery which " pkg))
       (s-trim it)
       (if (f-exists-p it)
           (find-file it)
         (user-error "Package not found, %s" pkg))))

(defun my-xdg-open-file-with-fd ()
  (interactive)
  (--> (shell-command-to-string "fd --type file ")
       (s-split "\n" it t)
       (completing-read "xdg-open: " it nil t)
       (call-process "xdg-open" nil 0 nil it)))

;;;; memo

(my-with-package dash)
(my-with-package s)
(require 'init-path)
(require 'dash)
(require 's)
(require 'org)
(require 'org-id)

(defun my--memo-source ()
  (--> (list "ag" "--nocolor" "--nogroup" "--file-search-regex" "\\.org$"
             "^\\* " (my-path-org-memo))
       (-map #'shell-quote-argument it)
       (s-join " " it)
       (shell-command-to-string it)
       (s-split "\n" it)
       (--map
        (-when-let ((_ file line title)
                    (s-match "\\`\\([^:]*\\):\\([0-9]+\\):\\* \\(.*\\)\\'" it))
          (propertize title 'file file 'line (string-to-number line)))
        it)
       (-non-nil it)))

(defun my--memo-file-and-line (str)
  (cons (get-text-property 0 'file str)
        (get-text-property 0 'line str)))

(defun my--memo-open-action (str)
  "Open memo indicated by STR from `completing-read'."
  (-let (((file . line) (my--memo-file-and-line str))
         (id (get-text-property 0 'id str)))
    (cond ((and file line)
           ;; backward link (file)
           (find-file file))
          (id
           ;; forward link (id)
           (find-file (car (org-id-find id))))
          (t (error "Cannot open %s" str)))))

(defun my-find-memo ()
  (interactive)
  (--> (completing-read "title: " (my--memo-source) nil t)
       (my--memo-open-action it)))

(defun my-insert-memo ()
  (declare (interactive-only t))
  (interactive)
  (--> (completing-read "title: " (my--memo-source) nil t)
       (-let (((file . line) (my--memo-file-and-line it)))
         (save-window-excursion
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (goto-char (point-min))
               (forward-line (1- line))
               (format "[[id:%s][%s]]"
                       (org-id-get-create)
                       ;; Remove tags from title.
                       (s-replace-regexp " +:\\(:?[^ :]+\\)+: *\\'" "" it))))))
       (insert it)))

(defun my--memo-search (id)
  (--> (list "ag" "--nocolor" "--nogroup" "--file-search-regex" "\\.org$"
             "--literal"
             (format "[id:%s]" id)
             (my-path-org-memo))
       (-map #'shell-quote-argument it)
       (s-join " " it)
       (prog1 it (message it))
       (shell-command-to-string it)
       (s-split "\n" it)
       (--map
        (-when-let ((_ file line)
                    (s-match "\\`\\([^:]*\\):\\([0-9]+\\):" it))
          (cons file (string-to-number line)))
        it)
       (-non-nil it)))

(defun my--memo-make-backlink-candidates (backlinks source)
  "Make backlink candidates for `completing-read'.
BACKLINKS is a returned value of `my--memo-search'.
SOURCE is a returned value of `my--memo-source'."
  (let ((title-table (make-hash-table :test #'equal)))
    ;; This depends on 1 memo per file.
    (--each source
      (-let (((file . _) (my--memo-file-and-line it)))
        (puthash file it title-table)))
    (--> backlinks
         (--map
          (-when-let* (((file . line) it)
                       (title (gethash file title-table)))
            (propertize title 'file file 'line line))
          it)
         (-non-nil it))))

(defun my--memo-search-links ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
        (cl-loop while (re-search-forward org-link-any-re nil t)
                 for title = (match-string 3)
                 for link = (match-string 2)
                 when (and title link (s-starts-with-p "id:" link))
                 collect (propertize
                          (substring-no-properties title)
                          'id (s-replace-regexp "^id:" "" link)))))))

(defun my-memo-related-links-to-this-file (file)
  (interactive (list (buffer-file-name)))
  (let (ids
        forward-links
        (memos (my--memo-source)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (save-restriction
          (widen)
          (org-map-region (lambda ()
                            (-some--> (org-id-get)
                              (push it ids)))
                          (point-min) (point-max))
          (setq ids (nreverse ids)
                forward-links (my--memo-search-links)))))
    (unless memos (error "There is no memo"))
    (--> (cl-loop for id in ids append (my--memo-search id))
         (my--memo-make-backlink-candidates it memos)
         (append it forward-links)
         (-uniq it)
         (prog1 it (unless it (error "Link not found")))
         (completing-read "link: " it nil t)
         (my--memo-open-action it))))

(defun my-memo-related-links-to ()
  (declare (interactive-only t))
  (interactive)
  (--> (completing-read "title: " (my--memo-source) nil t)
       (-let (((file . _) (my--memo-file-and-line it)))
         (my-memo-related-links-to-this-file file))))

(my-with-package ag)
(require 'ag)

(defun my-memo-search (query)
  (interactive "sQuery: ")
  (let ((ag-context-lines 1))
    (ag/search query (my-path-org-memo) :file-regex ".org$")))

(require 'org-capture)

(defun my-memo-capture ()
  (interactive)
  (declare (interactive-only t))
  (let* ((key "m")
         (org-capture-templates
          `((,key "Memo" entry
             (file
              ,(lambda ()
                 (--> (current-time)
                      (format-time-string "%Y/%m/%Y-%m-%d-%H%M%S.org" it)
                      (f-join (my-path-org-memo) it)
                      (prog1 it (make-directory (file-name-directory it) t)))))
             (file ,(my-path-org-capture-templates "basic.org"))
             :kill-buffer t
             :unnarrowed t))))
    (org-capture nil key)))

;; transient

(my-with-package transient)
(require 'transient)

(transient-define-prefix my-transient-memo ()
  :transient-non-suffix 'transient--do-exit
  [["Memo"
    ("c" "create" my-memo-capture)
    ("f" "find" my-find-memo)
    ("r" "find related" my-memo-related-links-to)
    ("R" "related to this file" my-memo-related-links-to-this-file)
    ("i" "insert" my-insert-memo)
    ("/" "search" my-memo-search)]]
  ["Transient"
   [("q" "quit" transient-quit-one)]
   [("<escape>" "quit" transient-quit-one)]])

;;;; file

(defun my-delete-buffer-file ()
  "Delete current buffer file."
  (interactive)
  (let ((file (buffer-file-name)))
    (and file
         (yes-or-no-p (format "Delete \"%s\"?" file))
         (progn
           (delete-file file)
           (yes-or-no-p "Kill this buffer?"))
         (kill-buffer))))

;;;; clone emacs

(defun my-clone-emacs ()
  "Start new emacs process with `load-path' inherited from current emacs."
  (interactive)
  (start-process
   "my-clone-emacs"
   " *Emacs Clone*"
   (cl-first command-line-args)
   "-q"
   "--eval"
   (format "%S"
           `(setq load-path ',load-path
                  custom-theme-load-path ',custom-theme-load-path))))

;;;; sequential-command like "convert backward word" commands

(my-with-package transient)
(require 'transient)

(defvar my-convert-word-continuously--start-position nil)

(defun my-convert-word-continuously-start-conversion ()
  (unless (markerp my-convert-word-continuously--start-position)
    (let ((marker (make-marker)))
      (set-marker marker (point))
      (setq my-convert-word-continuously--start-position marker))))

(defun my-convert-word-continuously-finish-conversion ()
  (when (and (markerp my-convert-word-continuously--start-position)
             (eq (current-buffer)
                 (marker-buffer my-convert-word-continuously--start-position)))
    (goto-char my-convert-word-continuously--start-position))
  (setq my-convert-word-continuously--start-position nil))

(transient-define-prefix my-convert-word-continuously-convert ()
  :transient-non-suffix
  (lambda ()
    (my-convert-word-continuously-finish-conversion)
    (transient--do-exit))
  [["Word conversion"
    ("M-u" "uppercase" my-convert-word-continuously-upcase-backward)
    ("M-l" "lowercase" my-convert-word-continuously-downcase-backward)
    ("M-c" "capitalize" my-convert-word-continuously-capitalize-backward)]]
  (interactive)
  ;; Always show the popup at the right side to prevent changing
  ;; visual position of the current line due to window height changing.
  (let ((transient-display-buffer-action
         `(,#'display-buffer-in-side-window
           (side . right))))
    (transient-setup 'my-convert-word-continuously-convert)))

(defun my-convert-word-continuously-upcase-backward ()
  (interactive)
  (declare (interactive-only t))
  (my-convert-word-continuously-start-conversion)
  (upcase-word -1)
  (backward-word 1)
  (call-interactively #'my-convert-word-continuously-convert))

(defun my-convert-word-continuously-downcase-backward ()
  (interactive)
  (declare (interactive-only t))
  (my-convert-word-continuously-start-conversion)
  (downcase-word -1)
  (backward-word 1)
  (call-interactively #'my-convert-word-continuously-convert))

(defun my-convert-word-continuously-capitalize-backward ()
  (interactive)
  (declare (interactive-only t))
  (my-convert-word-continuously-start-conversion)
  (capitalize-word -1)
  (backward-word 1)
  (call-interactively #'my-convert-word-continuously-convert))


(provide 'init-my-commands)
;;; init-my-commands.el ends here
