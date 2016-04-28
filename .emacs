(setq debug-on-error t)
(require 'cl)

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")

(defvar emacs-config-dir (concat root-dir "/.emacs.d/config/")
  "This directory houses all of the main configuration")

(defvar emacs-site-lisp-dir (concat root-dir "/.emacs.d/site-lisp/"))


(defvar emacs-snippet-dir (concat root-dir "/.emacs.d/snippets/")
  "This directory contains all snippets for yasnippet")

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

(require 'my-defaults)
(require 'my-packages)
(require 'my-auto-complete)
(require 'flex-isearch)
;(flex-isearch-mode t)
;(flex-isearch-auto t)
(require 'my-mmm)
(require 'my-ui)
(require 'my-yasnippet)
(require 'my-projectile)
(require 'my-grep)
(require 'my-javascript)
(require 'my-flycheck)
(require 'my-shell)
(require 'my-themes)
;;initialize global keybindings and mode mappings
(require 'my-keybindings)
(require 'my-mode-mappings)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;Load desktop last
(require 'my-desktop)

;; Always use subword mode (causes keys lke \M-f \m-b to operate over individual chunks of camel case words
(global-subword-mode t)

;; Show matching parens when after the paren
(require 'paren) (show-paren-mode t)


;; Window frame stuff
;; (setq default-frame-alist
;;                 '((top . 300) (left . 400)
;;                   (width . 100) (height . 50)
;;                   (cursor-color . "Orchid")
;; 		    (pointer-color . "Orchid")
;; 		      (border-color . "white")
;;                   (cursor-type . box)
;;                   (foreground-color . "white")
;;                   (background-color . "DarkSlateBlue")
;; 		    ))

;; (setq initial-frame-alist '((top . 10) (left . 30)))

;; (cond ((fboundp 'global-font-lock-mode)
;;        ;;Customize face attributes
;;        (setq font-lock-face-attributes
;;              ;; Symbol-for-Face Foreground Background Bold Italic Underline
;;              '((font-lock-comment-face        "OrangeRed")
;;                (font-lock-builtin-face        "LightSteelBlue")
;;                (font-lock-string-face         "lightsalmon")
;;                (font-lock-function-name-face  "LightSteelBlue")
;;                (font-lock-keyword-face        "Cyan")
;;                (font-lock-variable-name-face  "LightGoldenRod")
;;                (font-lock-type-face           "PaleGreen")
;;                (font-lock-constant-face       "aquamarine")
;;                ))
;;        ;; Load the font-lock package
;;        (require 'font-lock)
;;        ;; Maximum colors
;;        (setq font-lock-maximum-decoration t)
;;        ;; Turn on font-lock in all modes that support it
;;        (global-font-lock-mode t)))


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



;; ;;(normal-erase-is-backspace-mode t)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes (quote (wombat)))
;;  '(custom-safe-themes
;;    (quote
;;     ("f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" default)))
;;  '(fci-rule-color "#383838")
;;  '(nrepl-message-colors
;;    (quote
;;     ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
;;  '(vc-annotate-background "#2B2B2B")
;;  '(vc-annotate-color-map
;;    (quote
;;     ((20 . "#BC8383")
;;      (40 . "#CC9393")
;;      (60 . "#DFAF8F")
;;      (80 . "#D0BF8F")
;;      (100 . "#E0CF9F")
;;      (120 . "#F0DFAF")
;;      (140 . "#5F7F5F")
;;      (160 . "#7F9F7F")
;;      (180 . "#8FB28F")
;;      (200 . "#9FC59F")
;;      (220 . "#AFD8AF")
;;      (240 . "#BFEBBF")
;;      (260 . "#93E0E3")
;;      (280 . "#6CA0A3")
;;      (300 . "#7CB8BB")
;;      (320 . "#8CD0D3")
;;      (340 . "#94BFF3")
;;      (360 . "#DC8CC3"))))
;;  '(vc-annotate-very-old-color "#DC8CC3"))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )
