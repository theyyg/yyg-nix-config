;; emacs built-in backup settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Delete old copies
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; GDB many windows layout
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containging the main routine at startup
 gdb-show-main t
 )

;; Collapse code block; code folding
;; - 
;; set-selective-display
;; C-u xx C-$
;; -
;; hide-show
;; C-c @ C-c
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
	   (unless selective-display
		 (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
	  (if (condition-case nil
			  (hs-toggle-hiding)
			(error t))
		  (hs-show-all))
	(toggle-selective-display column)))

(load-library "hideshow")
(global-set-key (kbd "C-=") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)

(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; Yes and No from http://aaronbedra.com/emacs.d/
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll bar, Tool bar, Menu bar from http://aaronbedra.com/emacs.d/
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Marking and Selecting from http://aaronbedra.com/emacs.d/
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display Settings from http://aaronbedra.com/emacs.d/
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation from http://aaronbedra.com/emacs.d/
(setq tab-width 4
      indent-tabs-mode nil)

;; C-style
(setq c-default-style "google"
      c-basic-offset 4)

;; Key bindings from http://aaronbedra.com/emacs.d/
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)

;; Keystroke echo from http://aaronbedra.com/emacs.d/
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Key bindings to resize windows from https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Revert the current buffer (useful when switching between git branches)
(global-set-key (kbd "C-<escape>") 'revert-buffer)

;; Clear the entire current buffer (useful for resetting the shell buffer to track new content)
(global-set-key (kbd "C-M-<escape>") 'erase-buffer)

;; Code Navigation
(global-set-key (kbd "C-M-u") 'up-list)

;; Code formatting
(global-set-key (kbd "C-<tab>") 'clang-format-region)
(global-set-key (kbd "C-`") 'clang-format-buffer)
;; (setq clang-format-style

;; C indenting stuff
(defun my-c-mode-common-hook ()
  ;; (setq c-default-style "bsd" c-basic-offset 4)

  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2  ;; google-c-style.el is overriding this back to 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)

  (hl-line-mode t)
  (hs-minor-mode t)
  (lsp)  ;  LSP connection for auto-complete and code navigation
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)

;; JS indenting stuff
(defun my-js-mode-hook ()
  (setq indent-tabs-mode nil)  ; use spaces only if nil
  )

(add-hook 'js-mode-hook 'my-js-mode-hook)


;; Show column of cursor
(setq column-number-mode t)


;; Quick window switching
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'ace-window)

(add-hook 'c-mode-hook
	   (lambda () (modify-syntax-table ?\' ".")))

;;(load "folding" 'nomessage 'noerror)
;;(folding-mode-add-find-file-hook)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Powerline / telephone-line
;; Install from melpa
;; (require 'telephone-line)
(telephone-line-mode 1)

(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;; (when window-system

;;  (load-theme 'base16-woodland t))
;;; (load-theme 'base16-woodland t)

;; org mode todo states
(setq org-todo-keywords
	  '((sequence "TODO(t)" "ASSIGNED(a)" "IN PROGRESS(p)" "WAITING(w)" "WATCH(h)" "|" "FIXED(f)" "WILL NOT FIX(n)" "DUPLICATE(d)" "INTEGRATED(i)" "CLOSED(c)")))

;; org mode fix indentation in code blocks for export
(setq org-src-preserve-indentation t
      org-edit-src-content-indentation 2)

;; AGILE Board
;; "TODO" "In Progress" "Implemented" "Integrated" "Complete"
;; Bug Workflow
;; "NEW" "ASSIGNED" "WAITING" "IN WORK" "FIXED" "INTEGRATED" "CLOSED" "WAITING" "DUPLICATE" "WILL NOT FIX" "REOPENED"
;; Story Workflow
;; "DRAFT" "APPROVED BACKLOG" "IN PROGRESS" "IMPLEMENTED" "INTEGRATED" "VERIFIED" "COMPLETED" "BLOCKED" "REJECTED"

;; Saved for future reference
;; Separate C-i and tab
;; (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
;; 
;; ;; this is C-i
;; (global-set-key (kbd "C-i") (lambda () (interactive) (message "C-i"))) 
;; ;; this is <tab> key
;; (global-set-key (kbd "<tab>") (lambda () (interactive) (message "<tab>")))

;; Copy file::line to kill ring for pasting into org link
(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
for the current buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "%s::%d" (buffer-file-name) (save-restriction
										 (widen) (line-number-at-pos)))))
(global-set-key (kbd "M-`") 'position-to-kill-ring)

;; Set auto save files to an auto-save directory
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save") t))) 

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;;; Transparency

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(100 . 90))
(add-to-list 'default-frame-alist '(alpha . (100 . 90)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
		    ((numberp (cdr alpha)) (cdr alpha))
		    ;; Also handle undocumented (<active> <inactive>) form.
		    ((numberp (cadr alpha)) (cadr alpha)))
	      100)
	 '(95 . 80) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


;;;---------------------------------------------------------------------
;;; display-buffer

;; The default behaviour of `display-buffer' is to always create a new
;; window. As I normally use a large display sporting a number of
;; side-by-side windows, this is a bit obnoxious.
;;
;; The code below will make Emacs reuse existing windows, with the
;; exception that if have a single window open in a large display, it
;; will be split horisontally.
;; 
(setq pop-up-windows t)
(setq pop-up-frames nil)

;; (defun my-display-buffer-function (buf not-this-window)
;;   (if (and (not pop-up-frames)
;;            (one-window-p)
;;            (or not-this-window
;;                (not (eq (window-buffer (selected-window)) buf)))
;;            (> (frame-width) 162))
;;       (split-window-horizontally))
;;   ;; Note: Some modules sets `pop-up-windows' to t before calling
;;   ;; `display-buffer' -- Why, oh, why!
;;   (let ((display-buffer-function nil)
;;         (pop-up-windows nil))
;;     (display-buffer buf not-this-window)))
;; 
;; (setq display-buffer-function 'my-display-buffer-function)

;; (setq split-height-threshold 61)
;; (setq split-width-threshold 161)

(global-set-key (kbd "M-RET") 'magit-diff-visit-file-other-frame)

;; Org-mode C,C++ language support
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

;; Org-mode Python language support
(add-to-list 'org-src-lang-modes
  '("python" . python))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; plantUML for org-mode
;; (Execute this in a scratch buffer to enable plantuml: C-x C-e )
(setq org-plantuml-jar-path (expand-file-name "/home/local/MAGICLEAP/bwood/opt/bwood_arsenal/bin/plantuml.jar"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(defun init-plantuml()
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (setq org-plantuml-jar-path (expand-file-name "/home/local/MAGICLEAP/bwood/opt/bwood_arsenal/bin/plantuml.jar"))
  )

(add-hook 'org-mode-hook 'init-plantuml)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; Start-up with soft-wrap enabled
(setq org-startup-truncated nil)  ; This works
;; Toggle soft-wrap with super-q
(define-key org-mode-map (kbd "M-q") 'toggle-truncate-lines)  ; This also works


;; Emacs Ipython Notebook (in org-mode)
;; '((emacs-lisp . t) (ein . t))


;; Sublimity - smooth scrolling
(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 10
      sublimity-attractive-centering-width 110)

;(setq sublimity-scroll-vertical-frame-delay 0.01)

;; scroll one line at a time (less "jumpy" than defaults)
;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 101)
;(setq auto-window-vscroll nil)
;(setq scroll-margin 1
;      scroll-conservatively 10000
;      scroll-up-aggressively 0.01
;      scroll-down-aggressively 0.01)
;(setq-default scroll-up-aggressively 0.01
;              scroll-down-aggressively 0.01)

; Autosave every 500 typed characters
;(setq auto-save-interval 500)


;; Doom themes
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme
(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Enable the doom mode line
(doom-modeline-mode)
