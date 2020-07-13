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

(defun my-store-memo ()
  (declare (interactive-only t))
  (interactive)
  (--> (completing-read "title: " (my--memo-source) nil t)
       (-let (((file . line) (my--memo-file-and-line it)))
         (save-window-excursion
           (with-current-buffer (find-file file)
             (goto-char (point-min))
             (forward-line (1- line))
             (org-store-link nil t))))))

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

(autoload 'org-id-get "org-id" nil t)

(defun my-memo-backward-links ()
  (interactive)
  (let ((id (org-id-get))
        (memos (my--memo-source))
        (title-table (make-hash-table :test #'equal)))
    (unless id (error "ID not found on this tree"))
    (unless memos (error "There is no memo"))
    ;; This depends on 1 memo per file.
    (--each memos
      (-let (((file . _) (my--memo-file-and-line it)))
        (puthash file it title-table)))
    (--> (my--memo-search id)
         (--map
          (-when-let* (((file . line) it)
                       (title (gethash file title-table)))
            (propertize title 'file file 'line line))
          it)
         (-non-nil it)
         (completing-read "backlink: " it nil t)
         (my--memo-open-action it))))


(provide 'init-my-commands)
;;; init-my-commands.el ends here
