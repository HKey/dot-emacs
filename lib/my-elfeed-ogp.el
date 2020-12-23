;;; my-elfeed-ogp.el --- elfeed-ogp                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package elfeed)
(my-with-package dash)

(require 'elfeed-show)
(require 'dash)
(require 'time-date)                    ; for `seconds-to-time'

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

(defun my-elfeed-ogp-enable-new-entry-parsing ()
  (add-hook 'elfeed-new-entry-parse-hook
            #'my-elfeed-ogp--new-entry-parse-hook-fn))

(defun my-elfeed-ogp-disable-new-entry-parsing ()
  (remove-hook 'elfeed-new-entry-parse-hook
               #'my-elfeed-ogp--new-entry-parse-hook-fn))

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


(provide 'my-elfeed-ogp)
;;; my-elfeed-ogp.el ends here
