;;; config-ivy.el --- ivy                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package ivy)

(require 'ivy)


;;;; miscs
(setq ivy-extra-directories nil
      ivy-truncate-lines nil
      ;; stop starting completion with "^" with `org-refile', `woman'
      ;; and more
      ivy-initial-inputs-alist nil)

;;;; disable regexp of input query
(defun my-ivy-regex-plus-quoted (query)
  "Like `ivy--regex-plus' but disable regexp in QUERY."
  ;; using internal function is not good
  (ivy--regex-plus (regexp-quote query)))

(setq ivy-re-builders-alist
      `((t . ,#'my-ivy-regex-plus-quoted)))


(provide 'config-ivy)
;;; config-ivy.el ends here
