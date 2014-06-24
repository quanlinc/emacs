(add-to-list 'load-path "~/.emacs.d/")
(setq debug-on-error t)


(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")

(defvar emacs-config-dir (concat root-dir ".emacs.d/config/")
  "This directory houses all of the main configuration")

(add-to-list 'load-path emacs-config-dir)

;; Load modules
;(require 'my-auto-complete)
(require 'my-defaults)
(require 'my-desktop)
(require 'my-mmm)
;(require 'my-projectile)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)


(load-theme 'tango-dark t)

;;Hightlight the current line
(global-hl-line-mode 1)
;(set-face-background 'highlight "#9966cc")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight t)


;; Always use subword mode (causes keys lke \M-f \m-b to operate over individual chunks of camel case words
(global-subword-mode t)


;; First take care of UI
;; disable tool-bar/scroll-bar/menu-bar
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; Fullscreen mode
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value ( frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f12] 'toggle-fullscreen)

(blink-cursor-mode 0)

;;disable startup screen)
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; cursor settings
(set-cursor-color "grey60")



;;(load-file "~/.emacs.d/color-theme/themes/zenburn-theme.el")
;;(require 'zenburn-theme)
;; Allows Emacs to recognize cygwin paths
(require 'cygwin-mount)
(cygwin-mount-activate)

;; Set default file coding to be Unix style (needed in Cygwin)
(setq default-buffer-file-coding-system 'undecided-unix)

(setq max-specpdl-size 2000)

;; Display the column number on the status line
(setq column-number-mode "true")

(setq global-font-lock-mode "true")
(display-time)
(setq transient-mark-mode "true")

;; Show matching parens when after the paren
(require 'paren) (show-paren-mode t)

;; Shell stuff
(setq binary-process-input t) 
(setq w32-quote-process-args ?\") 
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 
(setq explicit-sh-args '("-login" "-i"))

;; Window frame stuff
(setq default-frame-alist
                '((top . 300) (left . 400)
                  (width . 100) (height . 50)
                  (cursor-color . "Orchid")
		    (pointer-color . "Orchid")
		      (border-color . "white")
                  (cursor-type . box)
                  (foreground-color . "white")
                  (background-color . "DarkSlateBlue")
		    ))

(setq initial-frame-alist '((top . 10) (left . 30)))


(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
				      (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
				      (inher-intro)
				         (case-label after)
					    (label after)
					       (access-label after)))
    (c-cleanup-list             . (scope-operator
				      empty-defun-braces
				         defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				      (substatement-open . 0)
				         (case-label        . 4)
					    (inline-open       . 0)
					       (block-open        . 0)
					          (statement-cont    . ++)
						     (knr-argdecl-intro . -)))
;;    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  (setq c-basic-offset 4)  
  (c-set-offset 'substatement-open 0)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro 0)

  ;; other customizations
  (setq tab-width 4
	;; this will make sure spaces are used instead of tabs
	indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
;;  (c-toggle-auto-hungry-state 0)

  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  )
     
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



     (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;   To invoke html-helper-mode automatically on .html files, do this:
    (setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.htm$" . html-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.jsp$" . html-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.jspx$" . html-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.C$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.java.in$" . java-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.eC$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.ec$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.ecin$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.eCin$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.Cin$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.cin$" . c++-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.fin$" . fortran-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.mak$" . makefile-mode) auto-mode-alist))


(defun indent-block (from to) 
  (interactive "r")
  (indent-rigidly from to tab-width))

(defun unindent-block (from to) 
  (interactive "r")
  (indent-rigidly from to (- tab-width)))

;; Make it easy to jump to next tag by hitting CTRL+.
(defun find-next-tag ()
  (interactive)
  (find-tag "" t))

;; Bind the CTRL plus . "dot" key to go to next tag
(global-set-key (kbd "C-.") 'find-next-tag)

;; SET UP FUNCTION KEYS
;;   CURRENTLY USED KEYS ARE: [F1]-[F12]  ([F2] defined below)
(global-set-key [f1] 'set-mark-command)
(global-set-key [f2] 'copy-region-as-kill)
(global-set-key [f3] 'undo)
(global-set-key [f4] 'replace-string)
(global-set-key [f5] 'query-replace)
(global-set-key [f6] 'isearch-forward-regexp)

;;(global-set-key [f7] 'isearch-backward-regexp) f7 has been mapped to expand templates
(global-set-key [f8] 'replace-regexp)
(global-set-key [f9] 'goto-line)
(global-set-key [f10] 'forward-sexp)
(global-set-key [f11] 'backward-sexp)
;;(global-set-key [f12] 'delete-char)
(global-set-key [f13] 'beginning-of-buffer)
(global-set-key [f14] 'end-of-buffer)

;; Set the key bindings for the console version of emacs
(global-set-key [kp-f1] 'set-mark-command)
(global-set-key [kp-f2] 'copy-region-as-kill)
(global-set-key [kp-f3] 'undo)
(global-set-key [kp-f4] 'replace-string)
(global-set-key [kp-f5] 'query-replace)
(global-set-key [kp-f6] 'isearch-forward-regexp)
(global-set-key [kp-f8] 'replace-regexp)
(global-set-key [kp-f9] 'goto-line)
(global-set-key [kp-f10] 'forward-sexp)
(global-set-key [kp-f11] 'backward-sexp)
(global-set-key [kp-f12] 'delete-char)
(global-set-key [kp-f13] 'beginning-of-buffer)
(global-set-key [kp-f14] 'end-of-buffer)

;; Abbreviations
;;(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved
;;(quietly-read-abbrev-file)       ;; reads the abbreviations file on startup

;; Load C++ templates - execute templates using F7 key
(load-file '"~/.emacs.d/tempo_skeletons.el")


;;(normal-erase-is-backspace-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
