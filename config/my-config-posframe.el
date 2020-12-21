;;; my-config-posframe.el --- posframe                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package posframe)

(require 'posframe)

;; prevent moving mouse cursor
(setq posframe-mouse-banish nil)
;; disable double buffering
(setq posframe-inhibit-double-buffering t)


(provide 'my-config-posframe)
;;; my-config-posframe.el ends here
