;; Keep my customizations in a file separate from the .emacs file.

(package-initialize)

(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; Add my own emacs directory to the load path.
;; (setq load-path
;;       (cons (condition-case () (expand-file-name "~/emacs") (error nil) )
;;             load-path) )

;; Keep my customizations in a file separate from the .emacs file.
(setq sandbox-file "~/.emacs-sandbox.el")
;; (load sandbox-file)
