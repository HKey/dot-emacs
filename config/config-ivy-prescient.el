;;; config-ivy-prescient.el --- ivy-prescient        -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package ivy-prescient)

(require 'ivy-prescient)

;; to use `my-ivy-regex-plus-quoted' as a default re builder,
;; do not overwrite `ivy-re-builders-alist'.
(setq ivy-prescient-enable-filtering nil)

(provide 'config-ivy-prescient)
;;; config-ivy-prescient.el ends here
