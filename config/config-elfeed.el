;;; config-elfeed.el --- elfeed                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(use-package elfeed)

(require 'elfeed)
(require 'elfeed-search)
(require 'elfeed-show)

;;; org-capture
(defun my-org-capture-elfeed ()
  "Capture current entry of elfeed"
  (interactive)
  (let* ((entry (elfeed-search-selected :single))
         (title (elfeed-entry-title entry))
         (url (elfeed-entry-link entry)))
    ;; pass title and url to org-capture via kill ring
    (kill-new
     (format "%s\n%s" title url))
    (org-capture nil "c")))

(define-key elfeed-search-mode-map (kbd "C") #'my-org-capture-elfeed)
(define-key elfeed-show-mode-map (kbd "C") #'my-org-capture-elfeed)


(provide 'config-elfeed)
;;; config-elfeed.el ends here
