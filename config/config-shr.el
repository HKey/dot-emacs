;;; config-shr.el --- shr                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'shr)

;;;; basic configuration

;; for eww and elfeed
(setq shr-width 70
      shr-use-colors nil
      shr-max-image-proportion 0.4
      ;; use variable pitch font
      shr-use-fonts t
      ;; disable animation, it makes emacs slow
      shr-image-animate nil
      ;; shr-inhibit-images t
      )

;;;; slice big images

(defface my-shr-image
  '((t :underline nil))
  "Disable underline for sliced images"
  :group 'shr)

(defun my-shr-put-image (spec alt &optional flags)
  "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type."
  ;; ref: `shr-put-image'
  (if (display-graphic-p)
      (let* ((size (cdr (assq 'size flags)))
             (data (if (consp spec)
                       (car spec)
                     spec))
             (content-type (and (consp spec)
                                (cadr spec)))
             (start (point))
             (image (cond
                     ((eq size 'original)
                      (create-image data nil t :ascent 100
                                    :format content-type))
                     ((eq content-type 'image/svg+xml)
                      (when (image-type-available-p 'svg)
                        (create-image data 'svg t :ascent 100)))
                     ((eq size 'full)
                      (ignore-errors
                        (shr-rescale-image data content-type
                                           (plist-get flags :width)
                                           (plist-get flags :height))))
                     (t
                      (ignore-errors
                        (shr-rescale-image data content-type
                                           (plist-get flags :width)
                                           (plist-get flags :height)))))))
        (when image
          ;; When inserting big-ish pictures, put them at the
          ;; beginning of the line.
          (when (and (> (current-column) 0)
                     (> (car (image-size image t)) 400))
            (insert "\n"))
          ;; --- modified start ---
          (let ((h (cdr (image-size image))))
            (insert-sliced-image image
                                 ;; Replace face to disable underline.
                                 ;; When it enabled, underline will be
                                 ;; shown at each sliced image.
                                 (propertize (or alt "*") 'face 'my-shr-image)
                                 nil (floor h) 1))
          ;; --- modified end ---
          (put-text-property start (point) 'image-size size)
          (when (and shr-image-animate
                     (cdr (image-multi-frame-p image)))
            (image-animate image nil 60)))
        image)
    (insert (or alt ""))))

(setq shr-put-image-function #'my-shr-put-image)


(provide 'config-shr)
;;; config-shr.el ends here
