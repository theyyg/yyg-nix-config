
(package-initialize)

;; Keep my packages in a file separate from the .emacs file.
(setq sandbox-file "~/.emacs-packages.el")
(load sandbox-file)

;; Keep my customizations in a file separate from the .emacs file.
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;; Location of test settings before they're added to emacs-custom.el
(setq sandbox-file "~/.emacs-sandbox.el")
(load sandbox-file)


