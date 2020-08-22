;;; config-browse-url.el --- browse-url              -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'browse-url)

;; Use xdg-open as default
(when (executable-find "xdg-open")
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; Use chrome and chromium in private browsing mode
(push "--incognito" browse-url-chromium-arguments)
(push "--incognito" browse-url-chrome-arguments)

(defun my-browse-url-switch-method (method)
  (interactive
   (list
    (intern-soft
     (completing-read "Method: "
                      '(browse-url-xdg-open
                        browse-url-firefox
                        browse-url-chromium
                        browse-url-chrome)
                      nil
                      t))))
  (when (functionp method)
    (setq browse-url-browser-function method)))


(provide 'config-browse-url)
;;; config-browse-url.el ends here
