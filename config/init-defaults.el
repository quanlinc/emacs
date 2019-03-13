;; Modifications to Emacs defaults setup

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)
;; Auto refresh buffers
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
;; Also auto refresh dired
(setq global-auto-revert-non-file-buffers t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Don't add unintentional new lines at the end of buffer
(setq next-line-add-newlines nil)

;; Always end a file with a newline
(setq require-final-newline t)

;; Disable backup/autosave
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; 120 characters is about how many before wrapping (with vertical split)
(setq fill-column 120)

;; Save a list of recent files visited.
(recentf-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Keep cursor away from edges when scrolling up/down
;(require 'smooth-scrolling)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(setq tramp-default-user "root")
;;(add-to-list 'tramp-default-proxies-alist
;;             '(nil "\\`root\\'" "/ssh:%h:"))
;;(add-to-list 'tramp-default-proxies-alist
;;             '((regexp-quote (system-name)) nil nil))
;; Root access on local host: /sudo::<path-to-root-owned-file>
;; Root access on remote hosts: /sudo:root@remote-host:<path-to-root-owned-file>

;; auto-completion in minibuffer
(icomplete-mode 1)

(winner-mode t)

;;ido mode
(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;;turn off alarm
;;by displaying a warning icon in the center of the screen
;;(setq visible-bell 1)

;;by flashing the modeline
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
;; ;;by flashing the screen
;; (setq lexical-binding t
;;       visible-bell nil
;;       ring-bell-function 'asc:flash-background)
;; (defun asc:flash-background ()
;;   (let ((fg (face-foreground 'default))
;;         (bg (face-background 'default)))
;;     (set-face-background 'default "DodgerBlue")
;;     (set-face-foreground 'default "black")
;;     (run-with-idle-timer
;;      1 nil (lambda ()
;;              (set-face-background 'default bg)
;;              (set-face-foreground 'default fg)))))


;;window move default key binding
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t)

;; Always use subword mode (causes keys lke \M-f \m-b to operate over individual chunks of camel case words
(global-subword-mode t)

;; Abbreviations
;;(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved
;;(quietly-read-abbrev-file)       ;; reads the abbreviations file on startup


;;(normal-erase-is-backspace-mode t)

(provide 'init-defaults)
