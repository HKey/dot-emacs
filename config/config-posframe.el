;;; config-posframe.el --- posframe                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package posframe)

(require 'posframe)

;; prevent moving mouse cursor
(setq posframe-mouse-banish nil)
;; disable double buffering
(setq posframe-inhibit-double-buffering t)


(provide 'config-posframe)
;;; config-posframe.el ends here
