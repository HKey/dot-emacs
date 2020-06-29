;;; init-path.el --- Path definitions               -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package f)

(require 'f)

;;;; utility
(defmacro my-defpath (name path &optional docstring)
  "Define a path variable and function."
  (declare (indent 1))
  `(progn
     (defvar ,name ,path ,docstring)
     (defun ,name (&rest args)
       ,docstring
       (apply #'f-join ,name args))))

;;;; path

(my-defpath my-path-dot-emacs-prj
  (expand-file-name ".." (f-dirname (f-this-file))))

;; org-mode
(my-defpath my-path-org (expand-file-name "~/org"))
(my-defpath my-path-org-agenda (my-path-org "gtd"))


(provide 'init-path)
;;; init-path.el ends here
