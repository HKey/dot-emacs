;;; init-path.el --- Path definitions               -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(use-package f)

(require 'f)

;;;; utility
(defmacro my-defpath (name path &optional docstring)
  "Define a path variable and function."
  `(progn
     (defvar ,name ,path ,docstring)
     (defun ,name (&rest args)
       ,docstring
       (apply #'f-join ,name args))))

;;;; path

(my-defpath my-path-org (expand-file-name "~/org"))




(provide 'init-path)
;;; init-path.el ends here