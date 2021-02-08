;;; my-config-ivy.el --- ivy                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package ivy)
(my-with-package counsel)
(my-with-package dash)
(my-with-package s)

(require 'ivy)
;; counsel.el pushes some config into `ivy-initial-inputs-alist' using
;; toplevel code.
;; So to prevent overwriting `ivy-initial-inputs-alist',
;; load counsel.el before setting `ivy-initial-inputs-alist' explicitly.
(require 'counsel)
(require 'dash)
(require 's)

;;;; key binding

(require 'my-util)

(my-define-key ivy-minibuffer-map
  "M-m" #'ivy-immediate-done ; (lookup-key ivy-minibuffer-map (kbd "C-M-j"))
  )

(my-define-key ivy-switch-buffer-map
  "C-k" nil)

;;;; miscs
(setq ivy-extra-directories nil
      ;; Prevent minibuffer growing when some candidates were wrapped.
      ivy-truncate-lines t
      ;; stop starting completion with "^" with `org-refile', `woman'
      ;; and more
      ivy-initial-inputs-alist nil
      ivy-height 14)

;;;; regex builder

;;;;; disable regexp of input query
(defun my-ivy-regex-quoted-orderless (query)
  "Like `ivy--regex-plus' but disable regexp in QUERY."
  (--> (s-split " " query)
       (-map #'regexp-quote it)
       (--map (cons it t) it)))

;;;;; migemoize input query
(my-with-package migemo)
(require 'migemo)

(defun my-ivy-migemo-orderless (query)
  "Make QUERY orderless and migemoized."
  (--> (s-split " " query)
       (-map #'migemo-get-pattern it)
       (--map (cons it t) it)))

;;;;; set re-builders

(require 'my-init-commands)
(require 'my-memo)

(my-with-package swiper)

(setq ivy-re-builders-alist
      `((,#'swiper . ,#'my-ivy-migemo-orderless)
        (,#'counsel-recentf . ,#'my-ivy-migemo-orderless)
        (,#'counsel-yank-pop . ,#'my-ivy-migemo-orderless)
        (,#'counsel-imenu . ,#'my-ivy-migemo-orderless)
        (,#'counsel-outline . ,#'my-ivy-migemo-orderless)
        (,#'counsel-org-goto . ,#'my-ivy-migemo-orderless)
        (,#'counsel-org-agenda-headlines . ,#'my-ivy-migemo-orderless)
        (,#'read-file-name-internal . ,#'my-ivy-migemo-orderless)
        (,#'my-xdg-open-file-with-fd . ,#'my-ivy-migemo-orderless)
        (,#'my-memo-find . ,#'my-ivy-migemo-orderless)
        (,#'my-memo-insert . ,#'my-ivy-migemo-orderless)
        (,#'my-memo-related-links-to-this-file . ,#'my-ivy-migemo-orderless)
        (,#'my-memo-related-links-to . ,#'my-ivy-migemo-orderless)
        (t . ,#'my-ivy-regex-quoted-orderless)))


(provide 'my-config-ivy)
;;; my-config-ivy.el ends here
