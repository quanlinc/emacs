;;; init-editing-utils.el --- Modifications to Emacs defaults setup
;;; Commentary:
;;; Code:

;;-------------------------
;; Personal preferences
;;-------------------------
(setq-default
 ;; the blinking cursor is nothing, but an annoyance
 blink-cursor-mode -1
 ;; set default bookmark file location
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 ;; set max buffer menu size so that I won't get into too many files open error
 buffers-menu-max-size 50
 ;; No splash screen please ... jeez
 inhibit-startup-message t
 ;; Always display line and column numbers
 column-number-mode t
 line-number-mode t
 ;; Don't create lock file please
 create-lockfiles nil
 ;; ediff layout
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; display time
 display-time-mode t
 ;; display file size
 size-indication-mode t
 ;; Never insert tabs
 indent-tabs-mode nil
 ;; Remove text in active region if inserting text
 delete-selection-mode 1
 ;; Real emacs knights don't use shift to mark things
 shift-select-mode nil
 ;; Move files to trash when deleting
 delete-by-moving-to-trash t
 ;; Show keystrokes in progress
 echo-keystrokes 0.1
 ;; Allow pasting selection outside of Emacs
 x-select-enable-clipboard t
 ;; Transparently open compressed files
 auto-compression-mode t
 ;; Enable syntax highlighting for older Emacsen that have it off
 global-font-lock-mode t
 ;; Disable backup/autosave
 make-backup-files nil
 backup-inhibited t
 auto-save-default nil
 ;; Choose one behavior to end a buffer
 ;; Don't add unintentional new lines at the end of buffer
 next-line-add-newlines nil)

;; Adjust gc-cons-threshold
;; The default setting is too low for lsp-mode's needs due to the fact that client/server communication generates a lot of memory/garbage
(setq gc-cons-threshold 100000000)

;; Always end a file with a newline
;;require-final-newline t

;;; New line behavior
(global-set-key (kbd "RET") 'newline-and-indent)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; Always use subword mode (causes keys lke \M-f \m-b to operate over individual chunks of camel case words
(global-subword-mode 1)
(with-eval-after-load 'subword
  (diminish 'subword-mode))

;; Company mode
(global-company-mode 1)
(with-eval-after-load 'company
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  (diminish 'company-mode))

;; Telephone line mode
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)

;; hide eldoc minor mode
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode))

(setq mode-require-final-newline nil)

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;; Page break lines

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))

(global-set-key (kbd "M-j") 'join-line)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; use avy with all buffer, frame and windows
(when (maybe-require-package 'avy)
  (setq avy-all-windows 'all-frames)
  (global-set-key (kbd "s-;") 'avy-goto-char-timer)
  (global-set-key (kbd "s-w") 'ace-window))

;; Bind the CTRL plus . "dot" key to go to next tag
(global-set-key (kbd "C-.") 'find-next-tag)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

;;Helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Folding
(when (maybe-require-package 'origami)
  (with-eval-after-load 'origami
    (define-key origami-mode-map (kbd "C-c C-h") 'origami-toggle-node)
    (define-key origami-mode-map (kbd "C-c f") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-c F") 'origami-toggle-all-nodes)))

(global-origami-mode 1)

;; TODO: seems to conflict with default parenthesis behavior from lisp.el
;; (when (maybe-require-package 'paredit)
;;   (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)
;;   (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook             'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook           'enable-paredit-mode)
;; )

;; Auto refresh buffers
(add-hook 'after-init-hook 'global-auto-revert-mode)
;; Also auto refresh dired
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
;;(electric-layout-mode t)


;; 120 characters is about how many before wrapping (with vertical split)
(setq fill-column 120)

;; Save a list of recent files visited.
(recentf-mode 1)


(when (fboundp 'linum-mode)
  (setq display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'linum-mode))

(when (maybe-require-package 'goto-line-preview)
  (global-set-key [remap goto-line] 'goto-line-preview)

  (when (fboundp 'linum-mode)
    (defun sanityinc/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers)))


;; auto-completion in minibuffer
(icomplete-mode 1)

;; auto-completion with camel case
(global-set-key  (kbd "M-/") 'hippie-expand)

;; windows
(winner-mode t)

;; Make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)

;;ido mode
(ido-mode t)
(maybe-require-package 'ido-completing-read+)
(setq ido-everywhere nil)
(ido-ubiquitous-mode t)
(setq ido-enable-flex-matching t)

;;flex-isearch
(require-package 'flex-isearch)

;; bind Alt/Meta + n/p to scroll page up/down
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))

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

;; Abbreviations
;;(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved
;;(quietly-read-abbrev-file)       ;; reads the abbreviations file on startup


;;(normal-erase-is-backspace-mode t)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

;;---------------
;; Expand region
;;---------------
(require-package 'expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)

;;---------------
;; Multi-cursor
;;---------------

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

;;---------------
;; Huge files
;;---------------
(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))

;; Beacon, never lose cursor again
(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (setq-default beacon-color "#db6df9")
  (add-hook 'after-init-hook 'beacon-mode))

;; trim spaces from end of line
(when (maybe-require-package 'ws-butler)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

;; Automatically prompt available key bindings
(require-package 'which-key)
(which-key-mode)
(which-key-setup-side-window-right)

;; (require-package 'guide-key)
;; (setq guide-key/guide-key-sequence t)
;; (add-hook 'after-init-hook 'guide-key-mode)
;; (after-load 'guide-key
;;   (diminish 'guide-key-mode))

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
