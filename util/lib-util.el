;;; lib-util.el --- util                             -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;;; define-key

(require 'cl-lib)

(defun my-define-key (keymap &rest key-definitions)
  "In KEYMAP, assign definitions to keys.
KEY-DEFINITIONS is a list like below:
  (KEY1 DEF1 KEY2 DEF2 ...)"
  (cl-loop for (key def) on key-definitions by #'cddr
           do (let ((key (if (and (stringp key)
                                  (not (string= key " ")))
                             (read-kbd-macro key)
                           key)))
                (define-key keymap key def))))
(put 'my-define-key 'lisp-indent-function 1)


(provide 'lib-util)
;;; lib-util.el ends here
