;;; config-edebug.el --- edebug                      -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'edebug)

;; Increase delay time to be re-continued.
;; When edebug is stopping at a break point, re-continueing is canceled
;; if I press any key.
(setq edebug-sit-for-seconds 10)


(provide 'config-edebug)
;;; config-edebug.el ends here
