;;; config-company.el --- company                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package company)

(require 'company)

;;; key binding

(require 'lib-util)

(my-define-key company-active-map
  "C-n" #'company-select-next-if-tooltip-visible-or-complete-selection
  "C-p" #'company-select-previous
  "C-h" nil
  "C-s" #'company-filter-candidates
  "M-n" nil                         ; for mozc-temp
  "M-p" nil                         ; to keep consistency with "M-n"
  )

(my-define-key company-search-map
  "C-n" #'company-search-repeat-forward
  "C-p" #'company-search-repeat-backward)

;;; config

(setq
 ;; auto start
 ;; company-idle-delay nil
 company-idle-delay 0.1
 ;; company-minimum-prefix-length 1
 ;; company-minimum-prefix-length nil

 ;; auto complete
 company-auto-commit #'company-explicit-action-p

 ;; require match
 ;; company-require-match nil

 ;; company-selection-wrap-around t
 company-tooltip-limit 30

 ;; search regexp transform
 company-search-regexp-function #'company-search-words-regexp)


(provide 'config-company)
;;; config-company.el ends here
