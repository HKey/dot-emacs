;;; config-elfeed.el --- elfeed                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(use-package elfeed)

(require 'elfeed)
(require 'elfeed-search)
(require 'elfeed-show)

;;; org-capture
(defun my-org-capture-elfeed-search ()
  "Capture current entry of elfeed-search"
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (title (elfeed-entry-title entry))
         (url (elfeed-entry-link entry)))
    ;; pass title and url to org-capture via kill ring
    (kill-new
     (format "%s\n%s" title url))
    (org-capture nil "c")))

(defun my-org-capture-elfeed-show ()
  "Capture current entry of elfeed-show"
  (interactive)
  (let* ((entry elfeed-show-entry)
         (title (elfeed-entry-title entry))
         (url (elfeed-entry-link entry)))
    ;; pass title and url to org-capture via kill ring
    (kill-new
     (format "%s\n%s" title url))
    (org-capture nil "c")))

(define-key elfeed-search-mode-map (kbd "C") #'my-org-capture-elfeed-search)
(define-key elfeed-show-mode-map (kbd "C") #'my-org-capture-elfeed-show)


(provide 'config-elfeed)
;;; config-elfeed.el ends here
