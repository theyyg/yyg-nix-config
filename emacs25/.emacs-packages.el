;; Add my own emacs directory to the load path.
;;(setq load-path
;;      (cons (condition-case () (expand-file-name "~/opt/emacs") (error nil) )
;;            load-path) )

										; List of packages
(setq package-list
	  '(ace-window
		org
		magit
		magit-gerrit
		magit-lfs
		magit-todos
		js2-mode
		js2-refactor
		xref-js2
		js2-highlight-vars
		company-tern
		rainbow-mode
		fontify-face
		company-c-headers
		markdown-mode
		telephone-line
		doom-themes
		doom-modeline
		highlight-context-line
		slack
		spotify
		sr-speedbar))

;; clang-format

;; Other packages to try out
;; dropbox e2wm emacs-setup eyebrowse fixmee flycheck rtags (company-rtags flycheck-rtags) flymake fontify-face format-all frame-purpose frame-mode/frames-only-mode github-browse-file gitignore-mode google google-c-style/magicleap-c-style google-translate handoff helm helpful hide-lines hide-mode-line highlight highlight-parentheses highlight-symbol hl-anything hl-todo howdoi howm i3wm imenu(already included) idomenu iedit import-js indent-guide indent-tools intel-hex-mode iplayer irony ivy swiper counsel ivy-rtags jenkins key-chord latex-math-preview man-commands markdown-preview markdown-toc miniedit moom msvc multi-term/multi-run multi-web-mode multi-column multiple-cursors mwim nameframe navi-mode night-owl-theme no-littering nocomments-mode nov nyan-mode oauth outrespace p4 page-break-lines podcaster powerline protobuf-mode prop-menu splitjoin sudo-edit sx wc-mode wiki-summary wolfram xkcd xcscope xterm-color company smart-yank
;; concepts to find packages for: python slack clang shell/bash completion discord spacemacs git-diffing

					; list of repositories
(setq package-archives
 '(("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa-latest" . "https://melpa.org/packages/")
   ("org"         . "http://orgmode.org/elpa/")
   ("sc"   . "http://joseito.republika.pl/sunrise-commander/")
   ;;("marmalade"   . "http://marmalade-repo.org/packages/")
   ("gnu"         . "http://elpa.gnu.org/packages/")))


					; activate all the packages (in particular autoloads)
(package-initialize)

					; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

					; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

