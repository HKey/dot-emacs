;;; my-config-ps-mule.el --- ps-mule                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'ps-mule)

;; - [emacs] 日本語を含むバッファの(postsctipt)ps印刷で文字化けを無くす - 綾小路龍之介の素人思考
;;   http://za.toypark.in/html/2009/09-28.html
(setq ps-multibyte-buffer 'non-latin-printer)

(provide 'my-config-ps-mule)
;;; my-config-ps-mule.el ends here
