;;; my-config-mozc.el --- mozc                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package mozc)

(require 'mozc)

;; - `echo-area'
;; - `overlay'
(setq mozc-candidate-style 'echo-area)


(provide 'my-config-mozc)
;;; my-config-mozc.el ends here
