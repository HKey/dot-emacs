;;; config-elfeed.el --- elfeed                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package elfeed)

(require 'elfeed)

;;;; basic configuration

(setq elfeed-curl-max-connections 4)

;;;; org-capture
(use-package dash)
(use-package s)

(require 'elfeed-search)
(require 'elfeed-show)
(require 'elfeed-db)
(require 'dash)
(require 's)

(defun my-org-capture-elfeed--stringify-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (url (elfeed-entry-link entry))
        (tags (-some--> (elfeed-entry-tags entry)
                (-remove-item 'unread it)
                (-map #'symbol-name it)
                (s-join ":" it)
                (s-wrap it ":" ":"))))
    (format "%s  %s\n%s" title (or tags "") url)))

(defun my-org-capture-elfeed-search ()
  "Capture current entry of elfeed-search"
  (interactive)
  ;; pass title and url to org-capture via kill ring
  (kill-new
   (my-org-capture-elfeed--stringify-entry (elfeed-search-selected :single)))
  (org-capture nil "c"))

(defun my-org-capture-elfeed-show ()
  "Capture current entry of elfeed-show"
  (interactive)
  ;; pass title and url to org-capture via kill ring
  (kill-new
   (my-org-capture-elfeed--stringify-entry elfeed-show-entry))
  (org-capture nil "c"))

(define-key elfeed-search-mode-map (kbd "C") #'my-org-capture-elfeed-search)
(define-key elfeed-show-mode-map (kbd "C") #'my-org-capture-elfeed-show)

;;;; eww

(use-package dash)
(require 'dash)

(defun my-elfeed-search-eww ()
  "Open entry of elfeed-search with eww"
  (interactive)
  (--> (elfeed-search-selected :single)
       (elfeed-entry-link it)
       (eww it)))

(defun my-elfeed-show-eww ()
  "Open entry of elfeed-show with eww"
  (interactive)
  (--> elfeed-show-entry
       (elfeed-entry-link it)
       (eww it)))

(define-key elfeed-search-mode-map (kbd "e") #'my-elfeed-search-eww)
(define-key elfeed-show-mode-map (kbd "e") #'my-elfeed-show-eww)


(provide 'config-elfeed)
;;; config-elfeed.el ends here
