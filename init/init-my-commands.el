;;; init-my-commands.el --- My utility commands   -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'init-configuration) ; for loading order

(require 's)
(require 'f)
(require 'dash)


(defun my-open-gentoo-ebuild (pkg)
  "Open ebuild file with equery."
  (interactive "sPackage: ")
  (--> (shell-command-to-string (concat "equery which " pkg))
       (s-trim it)
       (if (f-exists-p it)
           (find-file it)
         (user-error "Package not found, %s" pkg))))


(provide 'init-my-commands)
;;; init-my-commands.el ends here
