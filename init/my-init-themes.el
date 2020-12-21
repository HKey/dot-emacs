;;; my-init-themes.el --- themes                        -*- lexical-binding: t; -*-

(require 'my-bootstrap)

;;;; paper-theme

(my-with-package paper-theme)

;;;; spacemacs-common

(my-with-package spacemacs-theme)
;; config is in my-config-spacemacs-common

;;;; solarized

(my-with-package solarized-theme)
;; config is in my-config-solarized.el

;;;; base16-theme

(my-with-package base16-theme)

;;;; zenburn-theme

(my-with-package zenburn-theme)

;;;; labburn-theme

(my-with-package labburn-theme)

;;;; gruvbox-theme

(my-with-package gruvbox-theme)

;;;; doom-themes

(my-with-package doom-themes)

;;;; theme overriding

(my-with-package dash)
(require 'dash)

(defvar my-theme-overriding
  ;; ((THEME . ((FACE . (:PARAM VALUE ...)) ...) ...) ...)
  '((spacemacs-light
     (dired-directory :inherit unspecified :patch t))
    (solarized-light
     (link :inherit link-visited))
    (solarized-dark
     (link :inherit link-visited))
    (solarized-light-high-contrast
     (link :inherit link-visited))
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
    (--> (-partition 2 attributes)
         (--reject (eq (car it) :patch) it)
         (--each it
           (-let (((prop val) it))
             (setq default (plist-put default prop val)))))
    (apply #'set-face-attribute face nil default)))

(defun my-theme-override (&rest _)
  (cl-loop for (theme . conf) in my-theme-overriding
           if (or (null theme)
                  (cl-member theme custom-enabled-themes))
           do (--each conf
                (my-theme-override-face (car it) (cdr it)))))

(advice-add #'load-theme :after #'my-theme-override)

;;;; enable theme

(when (display-graphic-p)
  ;; (progn
  ;;   ;; spacemacs-light
  ;;   (require 'my-config-spacemacs-common)
  ;;   (load-theme 'spacemacs-light t))
  ;; (progn
  ;;   ;; solarized-light-high-contrast
  ;;   (require 'my-config-solarized)
  ;;   (load-theme 'solarized-light-high-contrast t))
  (progn
    ;; solarized-light
    (require 'my-config-solarized)
    (load-theme 'solarized-light t))
  ;; (progn
  ;;   (load-theme 'gruvbox-light-medium t))
  ;; (progn
  ;;   (require 'my-config-doom-themes)
  ;;   (load-theme 'doom-nova t))
  )


(provide 'my-init-themes)
;;; my-init-themes.el ends here
