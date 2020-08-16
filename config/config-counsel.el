;;; config-counsel.el --- counsel                    -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package ivy)
(use-package counsel)
(use-package f)
(use-package dash)

(require 'ivy)
(require 'counsel)
(require 'f)
(require 'dash)

;;;; separate filename and dirname of candidates
(defun my-counsel-recentf-transformer (str)
  "Format STR in 'FILENAME  DIRNAME'"
  (format "%-20s  %s"
          (--> (f-filename str)
               (if (equal "" it) "/" it))
          (or (f-dirname str) "")))

(ivy-configure #'counsel-recentf
  :display-transformer-fn #'my-counsel-recentf-transformer)

;;;; settings

(setq counsel-yank-pop-separator "\n---\n")


(provide 'config-counsel)
;;; config-counsel.el ends here
