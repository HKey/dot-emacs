;;; config-eww.el --- eww                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)

(require 'eww)

;;;; org-capture

(defun my-org-capture-eww ()
  "Capture current page of eww"
  (interactive)
  (let ((title (plist-get eww-data :title))
        (url (plist-get eww-data :url)))
    ;; pass title and url to org-capture via kill ring
    (kill-new
     (format "%s\n%s" title url))
    (org-capture nil "c")))

(define-key eww-mode-map (kbd "C") #'my-org-capture-eww)


(provide 'config-eww)
;;; config-eww.el ends here