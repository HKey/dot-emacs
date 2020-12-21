;;; my-util.el --- util                             -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;;; define-key

(require 'cl-lib)

(defun my-define-key (keymap &rest key-definitions)
  "In KEYMAP, assign definitions to keys.
KEY-DEFINITIONS is a list like below:
  (KEY1 DEF1 KEY2 DEF2 ...)"
  (declare (indent 1))
  (cl-loop for (key def) on key-definitions by #'cddr
           do (let ((key (if (and (stringp key)
                                  (not (string= key " ")))
                             (read-kbd-macro key)
                           key)))
                (define-key keymap key def))))


(provide 'my-util)
;;; my-util.el ends here
