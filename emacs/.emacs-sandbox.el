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

;; Rtags setup 
;; Add C-c r keybindings
(rtags-enable-standard-keybindings)
;; Enable rtags diagnostics
(setq rtags-autostart-diagnostics t)
;; Enable completions in rtags
(setq rtags-completions-enabled t)
;; Enable company-mode
(require 'company)
;; Add company-rtags to company-backends
(push 'company-rtags company-backends)
;; company-mode
(global-company-mode)
;; company-mode completionn key binding
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;; Add flycheck
;;(require 'flycheck-rtags)

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
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Show column of cursor
(setq column-number-mode t)

;; Doom themes
(require 'doom-themes)

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


;; Sublimity
;(require 'sublimity)
;(require 'sublimity-scroll)
;(require 'sublimity-map) ;; experimental
;(require 'sublimity-attractive)

;(sublimity-mode 1)


;; Quick window switching
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'ace-window)


;; Powerline / telephone-line
(require 'telephone-line)
(telephone-line-mode 1)


;; Code Navigation
(global-set-key (kbd "C-M-u") 'up-list)


;; Being Slack Integration
;; I'm using use-package and el-get and evil

(require 'websocket)

;;(el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "blainewood"
   :default t
   :client-id "281654565381.401703961154"
   :client-secret "7446ab91ace023932d03180027710f08"
   :token "xoxp-281654565381-281436856003-401158131441-e062f7fc5008d3477a553fdbc77c07aa"
   :subscribed-channels '(general ml-alerts random)
   :full-and-display-names t)

;;  (slack-register-team
;;   :name "magicleap"
;;   :default t
;;   :client-id "3791969152.402004108839"
;;   :client-secret "fd7a6207273a754146399d5cdb57f428"
;;   :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
;;   :subscribed-channels '(test-rename rrrrr)
;;   :full-and-display-names t)

;;
;;  (evil-define-key 'normal slack-info-mode-map
;;    ",u" 'slack-room-update-messages)
;;  (evil-define-key 'normal slack-mode-map
;;    ",c" 'slack-buffer-kill
;;    ",ra" 'slack-message-add-reaction
;;    ",rr" 'slack-message-remove-reaction
;;    ",rs" 'slack-message-show-reaction-users
;;    ",pl" 'slack-room-pins-list
;;    ",pa" 'slack-message-pins-add
;;    ",pr" 'slack-message-pins-remove
;;    ",mm" 'slack-message-write-another-buffer
;;    ",me" 'slack-message-edit
;;    ",md" 'slack-message-delete
;;    ",u" 'slack-room-update-messages
;;    ",2" 'slack-message-embed-mention
;;    ",3" 'slack-message-embed-channel
;;    "\C-n" 'slack-buffer-goto-next-message
;;    "\C-p" 'slack-buffer-goto-prev-message)
;;   (evil-define-key 'normal slack-edit-message-mode-map
;;    ",k" 'slack-message-cancel-edit
;;    ",s" 'slack-message-send-from-buffer
;;    ",2" 'slack-message-embed-mention
;;    ",3" 'slack-message-embed-channel)
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

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

