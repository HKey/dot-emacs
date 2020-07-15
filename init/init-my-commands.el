;;; init-my-commands.el --- My utility commands   -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package s)
(use-package f)
(use-package dash)

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

;;;; memo

(my-with-package dash)
(my-with-package s)
(require 'init-path)
(require 'dash)
(require 's)
(require 'org)
(require 'org-id)

(defun my--memo-source ()
  (--> (list "ag" "--nocolor" "--nogroup" "^\\* " (my-path-org-memo))
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
  (-let (((file . line) (my--memo-file-and-line str)))
    (with-current-buffer (find-file file)
      (goto-char (point-min))
      (forward-line (1- line)))))

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
           (with-current-buffer (find-file file)
             (goto-char (point-min))
             (forward-line (1- line))
             (format "[[id:%s][%s]]"
                     (org-id-get-create)
                     ;; Remove tags from title.
                     (s-replace-regexp " +:\\(:?[^ :]+\\)+: *\\'" "" it)))))
       (insert it)))

(defun my--memo-search (id)
  (--> (list "ag" "--nocolor" "--nogroup" "--literal"
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

(defun my-memo-backward-links ()
  (interactive)
  (let ((id (org-id-get))
        (memos (my--memo-source)))
    (unless id (error "ID not found on this tree"))
    (unless memos (error "There is no memo"))
    (--> (my--memo-search id)
         (my--memo-make-backlink-candidates it memos)
         (prog1 it (unless it (error "Backlink not found")))
         (completing-read "backlink: " it nil t)
         (my--memo-open-action it))))

(defun my-memo-backward-links-to-this-file ()
  (interactive)
  (let ((ids nil)
        (memos (my--memo-source)))
    (save-restriction
      (widen)
      (org-map-region (lambda ()
                        (-some--> (org-id-get)
                          (push it ids)))
                      (point-min) (point-max))
      (nreverse ids))
    (unless ids (error "ID not found in this file"))
    (unless memos (error "There is no memo"))
    (--> (cl-loop for id in ids append (my--memo-search id))
         (my--memo-make-backlink-candidates it memos)
         (prog1 it (unless it (error "Backlink not found")))
         (completing-read "backlink: " it nil t)
         (my--memo-open-action it))))

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

(defun my-memo-forward-links ()
  (interactive)
  (--> (my--memo-search-links)
       (prog1 it (unless it (error "Link not found")))
       (completing-read "forward link: " it nil t)
       (get-text-property 0 'id it)
       (org-id-goto it)))


(provide 'init-my-commands)
;;; init-my-commands.el ends here
