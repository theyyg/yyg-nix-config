;;; Emacs session dedicated to gdb
;;; Originally from Gilles on stackexchange
;;; https://emacs.stackexchange.com/questions/46637/run-gdb-from-the-command-line

;; I'm often editing source files in my main session, so revert them
;; here automatically.
(when (fboundp 'global-auto-revert-mode)
  (global-auto-revert-mode))

;; Make this session easier to exit.
(if (boundp 'confirm-kill-emacs)
    (setq confirm-kill-emacs nil))
(defun my-confirm-kill-emacs ()
  (y-or-n-p "Really exit Emacs gdb session? "))
(or (memq 'my-confirm-kill-emacs kill-emacs-query-functions)
    (setq kill-emacs-query-functions
          (append kill-emacs-query-functions '(my-confirm-kill-emacs))))
(defun clear-buffer-process-query-on-exit-flag ()
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
        (set-process-query-on-exit-flag proc nil))))
(defun clear-gdb-inferior-process-query-on-exit-flag ()
  (if (equal (process-name (get-buffer-process (current-buffer))) "gdb-inferior")
      (clear-buffer-process-query-on-exit-flag)))
(add-hook 'comint-exec-hook 'clear-gdb-inferior-process-query-on-exit-flag)
(defadvice gud-common-init
    (after gud-common-init-set-process-query-on-exit-flag activate)
  "Don't ask for confirmation if exiting Emacs during the debug session."
  (clear-buffer-process-query-on-exit-flag))

;; I shouldn't need this, but `gdb-many-windows' often bugs out, and
;; this is useful to manually fix the windows.
(defun make-window-dedicated (arg)
  (interactive "p")
  "Make this window dedicated to the buffer that it's currently displaying.
With a zero or negative prefix argument, make this window non-dedicated."
  (set-window-dedicated-p (selected-window) (> arg 0)))

;; Run gdb.
(let ((command-string (combine-and-quote-strings command-line-args-left)))
  (set-frame-name (concat "gdb: " command-string))
  (gdb (concat "gdb -i=mi --args " command-string)))
(setq command-line-args-left nil)

;; Set up a nice window configuration.
(gdb-many-windows)
