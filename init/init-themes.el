;;; init-themes.el --- themes                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;;; paper-theme

(use-package paper-theme)

;;;; spacemacs-common

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (setq spacemacs-theme-org-agenda-height nil
        spacemacs-theme-org-bold nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-priority-bold nil))

;;;; solarized

(use-package solarized
  :ensure solarized-theme
  :config
  (setq solarized-use-variable-pitch  nil
        solarized-use-less-bold       t
        solarized-height-minus-1      1.0
        solarized-height-plus-1       1.0
        solarized-height-plus-2       1.0
        solarized-height-plus-3       1.0
        solarized-height-plus-4       1.0
        solarized-scale-org-headlines nil))

;;;; base16-theme

(use-package base16-theme)

;;;; theme overriding

(use-package dash)
(require 'dash)

(defvar my-theme-overriding
  ;; ((THEME . ((FACE . (:PARAM VALUE ...)) ...) ...) ...)
  '((spacemacs-light
     (dired-directory :inherit unspecified :patch t))
    (solarized-dark-high-contrast
     (link :inherit link-visited))
    (base16-greenscreen
     (org-link :inherit link)
     (org-drawer :inherit font-lock-keyword-face)
     (outline-2 :inherit outline-1)
     (outline-3 :inherit outline-1)
     (outline-4 :inherit outline-1)
     (outline-5 :inherit outline-1)
     (outline-6 :inherit outline-1)
     (outline-7 :inherit outline-1)
     (outline-8 :inherit outline-1))))

(defun my-theme-override-face (face attributes)
  (let ((default
          (unless (plist-get attributes :patch)
            (cl-loop for it in face-attribute-name-alist
                     append (list (car it) 'unspecified)))))
    (--each (-partition 2 attributes)
      (-let (((prop val) it))
        (setq default (plist-put default prop val))))
    (apply #'set-face-attribute face nil default)))

(defun my-theme-override (&rest _)
  (cl-loop for (theme . conf) in my-theme-overriding
           if (or (null theme)
                  (cl-member theme custom-enabled-themes))
           do (--each conf
                (my-theme-override-face (car it) (cdr it)))))

(advice-add #'load-theme :after #'my-theme-override)

;;;; enable theme

(load-theme 'spacemacs-light)


(provide 'init-themes)
;;; init-themes.el ends here
