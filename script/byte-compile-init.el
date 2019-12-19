(require 'package)

(package-initialize)

(let* ((this-file (or (buffer-file-name) load-file-name))
       (prj-root (expand-file-name
                  (concat (file-name-directory this-file)
                          "../")))
       (init (expand-file-name (concat prj-root "init")))
       (config (expand-file-name (concat prj-root "config"))))
  (setq byte-compile-error-on-warn t
        load-path `(,@load-path ,init ,config)))
