;;; my-bootstrap.el --- bootstrap script             -*- lexical-binding: t; -*-

;; use `eval-and-compile' to install use-package when compiling
(eval-and-compile
  (require 'package)

  ;; add repositories
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

  (package-initialize)


  ;; package
  (defun my--with-package-ensure-install (package)
    (unless (package-installed-p package)
      (package-install package))
    (add-to-list 'package-selected-packages package))

  (my--with-package-ensure-install 'dash)

  (require 'dash)

  (defmacro my-with-package (package &rest args)
    (-let* ((err-sym (cl-gensym "err-"))
            ((&alist :no-install no-install
                     :when when
                     :after after
                     :pin pin
                     :init: init
                     :config: config)
             (--> (-partition-by-header #'keywordp args)
                  (--map (if (string-match-p ":.*:$" (symbol-name (car it)))
                             it
                           (cons (cl-first it) (cl-second it)))
                         it)))
            (init-form
             (when init
               `(condition-case-unless-debug ,err-sym
                    ,@init
                  (error
                   (warn (format "Error at `%s' init, %s"
                                 ',package
                                 ,err-sym))))))
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
               `(my--with-package-ensure-install ',package)))
            (pin-form
             (when pin
               `(add-to-list 'package-pinned-packages (cons ',package ,pin))))
            (after-form
             (-some--> (list init-form config-form)
               (-non-nil it)
               (if after
                   `(with-eval-after-load ',after ,@it)
                 `(progn ,@it))))
            (when-form
             (when after-form
               `(when ,when
                  ,after-form)))
            (package-form
             (-some--> (list pin-form install-form)
               (-non-nil it)
               `(eval-and-compile ,@it))))
      `(condition-case-unless-debug ,err-sym
           ,@(-non-nil (list package-form when-form))
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

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
