;;; config-package.el --- package                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'package)

;;;; async

(my-with-package async)
(require 'async-bytecomp)

(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages 'all)


(provide 'config-package)
;;; config-package.el ends here
