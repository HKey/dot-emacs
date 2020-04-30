;;; config-ivy.el --- ivy                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package ivy)
(use-package counsel)
(use-package dash)
(use-package s)

(require 'ivy)
;; counsel.el pushes some config into `ivy-initial-inputs-alist' using
;; toplevel code.
;; So to prevent overwriting `ivy-initial-inputs-alist',
;; load counsel.el before setting `ivy-initial-inputs-alist' explicitly.
(require 'counsel)
(require 'dash)
(require 's)


;;;; miscs
(setq ivy-extra-directories nil
      ivy-truncate-lines nil
      ;; stop starting completion with "^" with `org-refile', `woman'
      ;; and more
      ivy-initial-inputs-alist nil)

;;;; disable regexp of input query
(defun my-ivy-regex-quoted-orderless (query)
  "Like `ivy--regex-plus' but disable regexp in QUERY."
  (--> (s-split " " query)
       (-map #'regexp-quote it)
       (--map (cons it t) it)))

(setq ivy-re-builders-alist
      `((t . ,#'my-ivy-regex-quoted-orderless)))


(provide 'config-ivy)
;;; config-ivy.el ends here