;;; config-org-taskforecast.el --- org-taskforecast  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(my-with-package org-taskforecast)

(require 'org-taskforecast)

;;;; basic config

(require 'init-path)

(setq org-taskforecast-dailylist-file
        (my-path-org "org-taskforecast/%Y/%Y-%m/%Y-%m-%d.org")
        org-taskforecast-day-start 0400
        org-taskforecast-sections '(("0700" 0700)
                                    ("0900" 0900)
                                    ("1200" 1200)
                                    ("1300" 1300)
                                    ("1500" 1500)
                                    ("1700" 1700)
                                    ("1900" 1900)
                                    ("2100" 2100)
                                    ("2300" 2300)
                                    ("2500" 2500)
                                    ("2630" 2630)))

(add-hook 'org-taskforecast-list-mode-hook #'hl-line-mode)


(provide 'config-org-taskforecast)
;;; config-org-taskforecast.el ends here
