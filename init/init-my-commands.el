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

(defun my-memo-source ()
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

(defun my-find-memo ()
  (interactive)
  (--> (completing-read "title: " (my-memo-source) nil t)
       (let ((file (get-text-property 0 'file it))
             (line (get-text-property 0 'line it)))
         (with-current-buffer (find-file file)
           (goto-char (point-min))
           (forward-line (1- line))))))


(provide 'init-my-commands)
;;; init-my-commands.el ends here
