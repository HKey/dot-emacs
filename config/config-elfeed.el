;;; config-elfeed.el --- elfeed                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package elfeed)

(require 'elfeed)

;;;; basic configuration

(setq elfeed-curl-max-connections 4)

;;;; entry formatter, `elfeed-show-refresh-function'

(require 'elfeed-show)
(require 'time-date)                    ; for `seconds-to-time'

(defface my-elfeed-title
  '((t :height 1.5 :inherit variable-pitch :bold t))
  "Title face of elfeed-show"
  :group 'elfeed)

(defface my-elfeed-show-details
  '((t :height 0.8 :inherit variable-pitch))
  "Details face of elfeed-show"
  :group 'elfeed)

(defun my-elfeed-show-refresh ()
  "Show entry"
  ;; ref: `elfeed-show-refresh--mail-style'
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string "%Y-%m-%d %H:%M:%S" date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (erase-buffer)
    (insert (propertize (concat title "\n")
                        'face '(my-elfeed-title message-header-subject)))
    (insert
     (propertize
      (concat nicedate " / " feed-title "\n")
      'face '(my-elfeed-show-details message-header-other)))
    (when tags
      (insert
       (format
        (propertize "Tags: %s\n"
                    'face '(my-elfeed-show-details message-header-name))
        (propertize tagsstr
                    'face '(my-elfeed-show-details message-header-other)))))
    (insert "\n")
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))

(setq elfeed-show-refresh-function #'my-elfeed-show-refresh)

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

(define-key elfeed-search-mode-map (kbd "Co") #'my-org-capture-elfeed-search)
(define-key elfeed-show-mode-map (kbd "Co") #'my-org-capture-elfeed-show)

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

;;;; pocket-reader

(use-package pocket-reader
  :pin manual)

(require 'pocket-reader)

(define-key elfeed-search-mode-map (kbd "Cp") #'pocket-reader-elfeed-search-add-link)
(define-key elfeed-show-mode-map (kbd "Cp") #'pocket-reader-elfeed-entry-add-link)


(provide 'config-elfeed)
;;; config-elfeed.el ends here
