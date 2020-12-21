;;; my-config-url-cookie.el --- url-cookie              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'url-cookie)

;; for eww web browsing
;; do not accept cookies
(setq url-cookie-untrusted-urls '(".*"))


(provide 'my-config-url-cookie)
;;; my-config-url-cookie.el ends here
