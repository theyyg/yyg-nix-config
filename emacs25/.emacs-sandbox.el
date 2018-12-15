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

;; Yes and No from http://aaronbedra.com/emacs.d/
(defalias 'yes-or-no-p 'y-or-n-p)

;; Scroll bar, Tool bar, Menu bar from http://aaronbedra.com/emacs.d/
(scroll-bar-mode -1)
;;(tool-bar-mode -1)
;;(menu-bar-mode -1)

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

;; Code Navigation
(global-set-key (kbd "C-M-u") 'up-list)

;; C++ indenting stuff
(defun my-c-mode-common-hook ()
  ;; (setq c-default-style "bsd" c-basic-offset 4)

  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; use spaces only if nil
  (hl-line-mode t)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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

;; (require 'golden-ratio)
;; (golden-ratio-mode 1)
;; (add-to-list 'golden-ratio-extra-commands 'ace-window)

;;(load "folding" 'nomessage 'noerror)
;;(folding-mode-add-find-file-hook)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; Powerline / telephone-line
;; Install from melpa
(require 'telephone-line)
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

(when window-system
  (load-theme 'base16-woodland t))

(load-theme 'base16-woodland t)

;; org mode todo states
(setq org-todo-keywords
  '((sequence "TODO" "ASSIGNED" "IN WORK" "WAITING" "WATCH" "|" "FIXED" "WILL NOT FIX" "DUPLICATE" "INTEGRATED" "CLOSED")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-to-list 'load-path "~/opt/emacs-doom-themes/")

;; Doom themes
;(require 'doom-themes)

;; Global settings (defaults)
;(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
;(doom-themes-visual-bell-config)

;; Enable custom neotree theme
;(doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

;; Corrects (and improves) org-mode's native fontification.
;(doom-themes-org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

										; Package specific config
										; Config taken from [https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html]
										; Js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

										; js2-refactor
										; xref-js2
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; javascript friendly kill
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

										; company-tern
										; config from [https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html]
(require 'company)
(require 'company-tern)

(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

