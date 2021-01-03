;;; my-init-commands.el --- My utility commands   -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package s)
(my-with-package f)
(my-with-package dash)

;;;; buffer

(defun my-kill-this-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;;; gentoo

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

;;;; xdg-open

(require 's)
(require 'dash)

(defun my-xdg-open-file-with-fd ()
  (interactive)
  (--> (shell-command-to-string "fd --type file ")
       (s-split "\n" it t)
       (completing-read "xdg-open: " it nil t)
       (call-process "xdg-open" nil 0 nil it)))

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
  (transient-setup 'my-convert-word-continuously-convert))

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

(require 'my-always-recenter)

(cl-callf append my-always-recenter-ignore-commands
  (list #'my-convert-word-continuously-upcase-backward
        #'my-convert-word-continuously-downcase-backward
        #'my-convert-word-continuously-capitalize-backward))


(provide 'my-init-commands)
;;; my-init-commands.el ends here
