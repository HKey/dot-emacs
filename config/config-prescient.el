;;; config-prescient.el --- prescient                -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package prescient)

(require 'prescient)

(prescient-persist-mode 1)

(setq prescient-history-length 10000)


(provide 'config-prescient)
;;; config-prescient.el ends here
