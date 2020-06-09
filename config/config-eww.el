;;; config-eww.el --- eww                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(require 'eww)

;;;; key binding

(require 'lib-util)

(defun my-eww-copy-title ()
  (interactive)
  (let ((title (plist-get eww-data :title)))
    (kill-new title)))

(defun my-eww-copy-title-and-url ()
  (interactive)
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    (kill-new
     (format "%s\n%s" title url))))

(defun my-eww-copy-as-org-list ()
  (interactive)
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    (kill-new
     (format "- %s\n  %s" title url))))

(my-define-key eww-mode-map
  "j" #'scroll-up-line
  "k" #'scroll-down-line
  "H" #'eww-back-url
  "L" #'eww-forward-url
  "yy" #'my-eww-copy-title-and-url
  "yu" #'eww-copy-page-url
  "yt" #'my-eww-copy-title
  "yo" #'my-eww-copy-as-org-list)

;;;; org-capture

(defun my-org-capture-eww ()
  "Capture current page of eww"
  (interactive)
  ;; pass title and url to org-capture via kill ring
  (my-eww-copy-title-and-url)
  (org-capture nil "c"))

(define-key eww-mode-map (kbd "C") #'my-org-capture-eww)


(provide 'config-eww)
;;; config-eww.el ends here
