;; Keep my customizations in a file separate from the .emacs file.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;(setq custom-file "~/.emacs-custom.el")
;;(load custom-file)

;; Add my own emacs directory to the load path.
(setq load-path
      (cons (condition-case () (expand-file-name "~/emacs") (error nil) )
            load-path) )

(setq custom-file "~/.emacs-ml.el")
(load custom-file)

;; Keep my customizations in a file separate from the .emacs file.
(setq sandbox-file "~/.emacs-sandbox.el")
(load sandbox-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(flymake-google-cpplint-command "/usr/local/bin/cpplint")
 '(flymake-google-cpplint-filter "--filter=-whitespace/line_length,-build")
 '(flymake-google-cpplint-verbose "--verbose=0")
 '(package-selected-packages
   (quote
    (company-lsp cquery lsp-mode clang-format realgud google-c-style flymake-google-cpplint flymake-cursor use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
