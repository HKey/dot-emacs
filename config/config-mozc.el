;;; config-mozc.el --- mozc                          -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package mozc)

(require 'mozc)

;; - `echo-area'
;; - `overlay'
(setq mozc-candidate-style 'echo-area)


(provide 'config-mozc)
;;; config-mozc.el ends here
