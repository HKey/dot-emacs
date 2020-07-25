;;; my-bootstrap.el --- bootstrap script             -*- lexical-binding: t; -*-

;; use `eval-and-compile' to install use-package when compiling
(eval-and-compile
  (require 'package)

  ;; add repositories
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  ;; pinning
  (setq package-pinned-packages
        '((org-taskforecast . "manual")
          (pocket-reader . "manual")))

  (package-initialize)


  ;; package
  (defvar my--with-package-refreshed-p nil)
  (defun my--with-package-ensure-install (package)
    (unless (package-installed-p package)
      (unless my--with-package-refreshed-p
        (package-refresh-contents)
        (setq my--with-package-refreshed-p t))
      (package-install package))
    (add-to-list 'package-selected-packages package))

  (my--with-package-ensure-install 'dash)

  (require 'dash)

  (defmacro my-with-package (package &rest args)
    (declare (indent 1))
    (-let* ((err-sym (cl-gensym "err-"))
            ((&alist :no-install no-install
                     :when when
                     :after after
                     :config: config)
             (--> (-partition-by-header #'keywordp args)
                  (--map (if (string-match-p ":.*:$" (symbol-name (car it)))
                             it
                           (cons (cl-first it) (cl-second it)))
                         it)))
            (config-form
             (when config
               `(with-eval-after-load ',package
                  (condition-case-unless-debug ,err-sym
                      ,@config
                    (error
                     (warn (format "Error at `%s' config, %s"
                                   ',package
                                   ,err-sym)))))))
            (install-form
             (unless no-install
               `(eval-and-compile
                  (my--with-package-ensure-install ',package))))
            (after-form
             (--when-let config-form
               (if after
                   `(with-eval-after-load ',after ,@it)
                 `(progn ,@it))))
            (when-form
             (when after-form
               `(when ,when
                  ,after-form))))
      `(condition-case-unless-debug ,err-sym
           ,@(-non-nil (list install-form when-form))
         (error
          (warn (format "Error at `%s' my-with-package form, %s"
                        ',package
                        ,err-sym))))))

  ;; use-package

  (unless (package-installed-p 'use-package)
    (when (yes-or-no-p "`use-package' package is missing, install it?: ")
      (package-refresh-contents)
      (package-install 'use-package))))

;;; use-package

(require 'use-package)

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-verbose t)

;;; `load-prefer-newer'

;; Always load newer file to avoid loading old files in byte compiling.
(setq load-prefer-newer t)

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
