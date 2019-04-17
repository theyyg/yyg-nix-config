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

;; Code formatting
(global-set-key (kbd "C-<tab>") 'clang-format-region)
;; (setq clang-format-style

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
;;; (load-theme 'base16-woodland t)

;; org mode todo states
(setq org-todo-keywords
	  '((sequence "TODO(t)" "ASSIGNED(a)" "IN PROGRESS(p)" "WAITING(w)" "WATCH(h)" "|" "FIXED(f)" "WILL NOT FIX(n)" "DUPLICATE(d)" "INTEGRATED(i)" "CLOSED(c)")))

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


;; Set auto save files to an auto-save directory
(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save") t))) 

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))


;;; Transparency

;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

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
	 '(85 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
