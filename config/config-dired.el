;;; config-dired.el --- dired                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(require 'dired)
;; dired-x overwrites `dired-mode-map' so load it before binding keys.
(require 'dired-x)

;;;; setting

(setq dired-dwim-target t
      dired-clean-up-buffers-too nil    ; do not delete orphan buffers
      dired-listing-switches "-AFhlv --time-style=long-iso"

      ;; Do not ask to copy/delete directories.
      dired-recursive-copies 'always
      dired-recursive-deletes 'always)

;;;; open with external application

(defun my-dired-open-file ()
  "In dired, open the file named on this line."
  (declare (interactive-only t))
  (interactive)
  (let ((file (dired-get-filename)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

;;;; sort

(defun my-dired-sort-by (switches)
  ;; refer `dired-sort-toggle-or-edit'
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (dired-sort-other
   (concat dired-listing-switches " " switches)))

(defun my-dired-sort-by-size ()
  (declare (interactive-only t))
  (interactive)
  (my-dired-sort-by "-S"))

(defun my-dired-sort-by-mtime ()
  (declare (interactive-only t))
  (interactive)
  (my-dired-sort-by "-t"))

(defun my-dired-sort-by-name ()
  (declare (interactive-only t))
  (interactive)
  (my-dired-sort-by "-v"))

(defun my-dired-sort-by-extension ()
  (declare (interactive-only t))
  (interactive)
  (my-dired-sort-by "-X"))

;;;; key bindings

(use-package dired-atool)

(require 'lib-util)

(my-define-key dired-mode-map
  "r" #'wdired-change-to-wdired-mode
  "I" #'dired-kill-subdir
  "F" #'my-dired-open-file

  ;; ranger like key bindings
  "h" (kbd "^")
  "l" (kbd "f")

  ;; sort
  "s" nil
  "ss" #'my-dired-sort-by-size
  "st" #'my-dired-sort-by-mtime
  "sn" #'my-dired-sort-by-name
  "sx" #'my-dired-sort-by-extension

  ;; dired-atool
  "z" #'dired-atool-do-unpack-with-subdirectory
  "Z" #'dired-atool-do-pack)

;;;; truncate-lines

(defun my-dired-truncate-lines ()
  (setq-local truncate-lines t))
(add-hook 'dired-mode-hook #'my-dired-truncate-lines)

;;;; auto-revert-mode

(add-hook 'dired-mode-hook #'auto-revert-mode)

;;;; hide details as default

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(setq dired-hide-details-hide-symlink-targets nil)


(provide 'config-dired)
;;; config-dired.el ends here
