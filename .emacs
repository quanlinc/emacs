(setq debug-on-error t)
(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")

(defvar emacs-config-dir (concat root-dir "/.emacs.d/config/")
  "This directory houses all of the main configuration")

(defvar emacs-site-lisp-dir (concat root-dir "/.emacs.d/site-lisp/"))

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)



(require 'my-auto-complete)
(require 'my-defaults)
(require 'my-desktop)
(require 'my-mmm)
(require 'my-projectile)
(require 'my-javascript)
(require 'my-keybindings)

(load-theme 'tango-dark t)
;;Hightlight the current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#3e4446")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight nil)


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
;;(require 'cygwin-mount)
;;(cygwin-mount-activate)

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

;; Abbreviations
;;(setq save-abbrevs t)              ;; save abbrevs when files are saved
                                     ;; you will be asked before the abbreviations are saved
;;(quietly-read-abbrev-file)       ;; reads the abbreviations file on startup

;; Load C++ templates - execute templates using F7 key
;;(load-file '"~/.emacs.d/tempo_skeletons.el")


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


