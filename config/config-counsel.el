;;; config-counsel.el --- counsel                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package ivy)
(use-package counsel)
(use-package f)

(require 'ivy)
(require 'counsel)
(require 'f)

;; separate filename and dirname of candidates
(defun my-counsel-recentf-transformer (str)
      "Format STR in 'FILENAME  DIRNAME'"
      (format "%-16s  %s"
              (f-filename str)
              (f-dirname str)))

(ivy-set-display-transformer 'counsel-recentf #'my-counsel-recentf-transformer)


(provide 'config-counsel)
;;; config-counsel.el ends here
