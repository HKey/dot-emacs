;;; config-solarized.el --- solarized                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package solarized-theme)

(require 'solarized)

(setq solarized-use-variable-pitch  nil
        solarized-use-less-bold       t
        solarized-height-minus-1      1.0
        solarized-height-plus-1       1.0
        solarized-height-plus-2       1.0
        solarized-height-plus-3       1.0
        solarized-height-plus-4       1.0
        solarized-scale-org-headlines nil)


(provide 'config-solarized)
;;; config-solarized.el ends here
