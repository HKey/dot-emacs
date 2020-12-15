;;; config-elfeed.el --- elfeed                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package elfeed)

(require 'elfeed)

;;;; basic configuration

(setq elfeed-curl-max-connections 4)

;;;; key binding

(require 'lib-util)
(require 'elfeed-show)

(my-define-key elfeed-show-mode-map
  "j" #'scroll-up-line
  "k" #'scroll-down-line)

;;;; entry formatter, `elfeed-show-refresh-function'

(my-with-package dash)

(require 'elfeed-show)
(require 'time-date)                    ; for `seconds-to-time'
(require 'dash)

(defface my-elfeed-show-details
  '((t :height 0.8 :inherit variable-pitch))
  "Details face of elfeed-show"
  :group 'elfeed)

(defvar my-elfeed-ogp-image-table (make-hash-table :test #'equal))

(defvar my-elfeed-ogp--link-stack nil)

(defvar my-elfeed-ogp--parser-timer nil)

(defun my-elfeed-ogp-set-image-url (link image-url)
  (puthash link image-url my-elfeed-ogp-image-table))

(defun my-elfeed-ogp-get-image-url (link)
  (unless (my-elfeed-ogp-article-registered-p link)
    (my-elfeed-ogp-parse-link link))
  (gethash link my-elfeed-ogp-image-table))

(defun my-elfeed-ogp-article-registered-p (link)
  (let ((nothing (cl-gensym)))
    (not (eq (gethash link my-elfeed-ogp-image-table nothing)
             nothing))))

(defun my-elfeed-ogp-parse-link (link)
  (my-elfeed-ogp-load-as-needed)
  (unless (my-elfeed-ogp-article-registered-p link)
    (-->
     (-some-->
         (with-temp-buffer
           (url-insert-file-contents link)
           (libxml-parse-html-region (point-min) (point-max)))
       (dom-by-tag it 'meta)
       (--find (equal "og:image" (dom-attr it 'property)) it)
       (dom-attr it 'content)
       (url-expand-file-name it link))
     (my-elfeed-ogp-set-image-url link it))))

(defun my-elfeed-ogp--run-parser ()
  (unless my-elfeed-ogp--parser-timer
    (setq my-elfeed-ogp--parser-timer
          (run-with-timer
           2 nil
           (lambda ()
             (setq my-elfeed-ogp--link-stack
                   (-drop-while #'my-elfeed-ogp-article-registered-p
                                my-elfeed-ogp--link-stack))
             (-when-let (url (pop my-elfeed-ogp--link-stack))
               (message "Parsing %s... (rest: %s entries)"
                        url
                        (length my-elfeed-ogp--link-stack))
               (my-elfeed-ogp-parse-link url)
               (message "Parsing %s... done (rest: %s entries)"
                        url
                        (length my-elfeed-ogp--link-stack)))
             (setq my-elfeed-ogp--parser-timer nil)
             (when my-elfeed-ogp--link-stack
               (my-elfeed-ogp--run-parser)))))))

(defun my-elfeed-ogp-stop-parser ()
  (interactive)
  (when (timerp my-elfeed-ogp--parser-timer)
    (cancel-timer my-elfeed-ogp--parser-timer))
  (setq my-elfeed-ogp--link-stack nil))

(defun my-elfeed-ogp-push-link (link)
  (unless (my-elfeed-ogp-article-registered-p link)
    (cl-pushnew link my-elfeed-ogp--link-stack :test #'equal)
    (my-elfeed-ogp--run-parser)))

(defun my-elfeed-ogp--new-entry-parse-hook-fn (_type _xml entry)
  (my-elfeed-ogp-load-as-needed)
  (when (time-less-p (time-subtract (current-time) (* 60 60 24 30))
                     (elfeed-entry-date entry))
    (my-elfeed-ogp-push-link (elfeed-entry-link entry))))

(add-hook 'elfeed-new-entry-parse-hook
          #'my-elfeed-ogp--new-entry-parse-hook-fn)

;; save/restore

(defvar my-elfeed-ogp-save-file
  (expand-file-name ".elfeed-ogp" user-emacs-directory))

(defun my-elfeed-ogp-save ()
  (with-temp-file my-elfeed-ogp-save-file
    (insert (prin1-to-string my-elfeed-ogp-image-table))))

(defun my-elfeed-ogp-add-save-hook ()
  (add-hook 'kill-emacs-hook #'my-elfeed-ogp-save))

(defun my-elfeed-ogp-load ()
  (when (file-exists-p my-elfeed-ogp-save-file)
    (with-temp-buffer
      (insert-file-contents my-elfeed-ogp-save-file)
      (let ((table (read (buffer-string))))
        (when (hash-table-p table)
          (setq my-elfeed-ogp-image-table table))))))

(defun my-elfeed-ogp-load-as-needed ()
  (my-elfeed-ogp-add-save-hook)
  (when (and (= 0 (hash-table-count my-elfeed-ogp-image-table))
             (file-exists-p my-elfeed-ogp-save-file))
    (my-elfeed-ogp-load)))


(defun my-elfeed-show-refresh ()
  "Show entry"
  ;; ref: `elfeed-show-refresh--mail-style'
  (interactive)
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (link (elfeed-entry-link elfeed-show-entry))
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
    ;; To wrap words with `shr-width', use `shr-insert-document' and
    ;; "h1" element.
    (shr-insert-document `(h1 nil ,title))
    (insert "\n")
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
    (when t
      (-some--> (my-elfeed-ogp-get-image-url link)
        (shr-insert-document `(img ((src . ,it))))))
    ;; (setq-local shr-inhibit-images t)
    (if content
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic)))
    (goto-char (point-min))))

(setq elfeed-show-refresh-function #'my-elfeed-show-refresh)

;;;; org-capture
(my-with-package dash)
(my-with-package s)

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

(my-with-package dash)
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

(my-with-package pocket-reader)
(require 'pocket-reader)

(define-key elfeed-search-mode-map (kbd "Cp") #'pocket-reader-elfeed-search-add-link)
(define-key elfeed-show-mode-map (kbd "Cp") #'pocket-reader-elfeed-entry-add-link)

;;;; feed-discovery

(my-with-package feed-discovery)


(provide 'config-elfeed)
;;; config-elfeed.el ends here
