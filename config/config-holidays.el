;;; config-holidays.el --- holidays                  -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'holidays)

;;;; marking

;; mark holidays
(setq calendar-mark-holidays-flag t)

;; mark today
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

;;;; japanese-holidays

(use-package japanese-holidays)
(require 'japanese-holidays)

;; holidays
(setq calendar-holidays
      (append japanese-holidays
              holiday-local-holidays
              holiday-other-holidays))

;; mark weekends
(add-hook 'calendar-today-visible-hook #'japanese-holiday-mark-weekend)
(add-hook 'calendar-today-invisible-hook #'japanese-holiday-mark-weekend)


(provide 'config-holidays)
;;; config-holidays.el ends here
