;;; config-shr.el --- shr                            -*- lexical-binding: t; -*-

(require 'my-bootstrap)
(require 'shr)

;;;; basic configuration

;; for eww and elfeed
(setq shr-width 60
      shr-use-colors nil
      ;; shr-max-image-proportion 0.4
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

(defun my-shr-rescale-image (data content-type width height
                                  &optional max-width max-height)
  "Rescale DATA, if too big, to fit the current buffer.
WIDTH and HEIGHT are the sizes given in the HTML data, if any.

The size of the displayed image will not exceed
MAX-WIDTH/MAX-HEIGHT.  If not given, use the current window
width/height instead."
  ;; if (not (get-buffer-window (current-buffer) t))
  ;; (create-image data nil t :ascent 100)
  (let* ((edges (window-inside-pixel-edges
                 ;; (get-buffer-window (current-buffer))
                 (selected-window)))
         ;; (max-width (truncate (* shr-max-image-proportion
         ;;                         (or max-width
         ;;                             (- (nth 2 edges) (nth 0 edges))))))
         ;; (max-height (truncate (* shr-max-image-proportion
         ;;                          (or max-height
         ;;                              (- (nth 3 edges) (nth 1 edges))))))
         (max-width (truncate
                     (or max-width
                         (min (* shr-width (default-font-width))
                              (- (nth 2 edges) (nth 0 edges))))))
         (max-height (truncate
                      (or max-height
                          (min (* 10 (default-font-height))
                               (- (nth 3 edges) (nth 1 edges))))))
         (scaling (image-compute-scaling-factor image-scaling-factor)))
    (when (or (and width
                   (> width max-width))
              (and height
                   (> height max-height)))
      (setq width nil
            height nil))
    (if (and
         ;; or
         width height
         (< (* width scaling) max-width)
         (< (* height scaling) max-height))
        (create-image
         data (shr--image-type) t
         :ascent 100
         :width width
         :height height
         :format content-type)
      (create-image
       data (shr--image-type) t
       :ascent 100
       :width width
       :height height
       :max-width max-width
       :max-height max-height
       :format content-type))))

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
                        (my-shr-rescale-image data content-type
                                              (plist-get flags :width)
                                              (plist-get flags :height))))
                     (t
                      (ignore-errors
                        (my-shr-rescale-image data content-type
                                              (plist-get flags :width)
                                              (plist-get flags :height)))))))
        (when (and image
                   ;; gif makes emacs slow
                   (not (image-multi-frame-p image)))
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
;; (setq shr-put-image-function #'shr-put-image)


(provide 'config-shr)
;;; config-shr.el ends here
