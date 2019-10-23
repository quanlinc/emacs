;;; init.el --- Initialization file for configuration
;;; Commentary:
;;; Code:

;; For debugging error on startup
;; (setq debug-on-error t)

;; toggle the default spelling check feature
(defconst *spell-check-support-enalbed* t)
(defconst *is-a-mac* (eq system-type 'darwin))

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")
(defvar emacs-config-dir (concat root-dir "/config/")
  "This directory houses all of the main configuration")
(defvar emacs-site-lisp-dir (concat root-dir "/site-lisp/")
  "This directory houses other extensions and git submodules")
(defvar emacs-snippet-dir (concat root-dir "/snippets/")
  "This directory contains all snippets for yasnippet")


(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

(setenv "PATH" (concat (getenv "PATH") ":/Users/quanlin.chen/.nvm/versions/node/v10.7.0/bin"))
(setq exec-path (append exec-path '("/Users/quanlin.chen/.nvm/versions/node/v10.7.0/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(require 'init-benchmark)
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;-----------------------
;; Fire up emacs
;;-----------------------
(require 'init-packages)
(require 'init-utils)
(require-package 'diminish)
(require 'init-editing-utils)
(require 'init-dired)
;;(require 'init-auto-complete)
(require 'flex-isearch)
(require 'init-company)
(require 'init-mmm)
(require 'init-gui-frames)
(require 'init-themes)
(require 'init-frame-hooks)

(require 'init-uniquify)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-grep)
(require 'init-tramp)
(require 'helm-config)

(require 'init-flycheck)
(require 'init-lsp)
;; programming style
(require 'init-lisp)
(require 'init-html)
(require 'init-css)
(require 'init-java)
(require 'init-javascript)
(require 'init-jsdoc)
(require 'init-terraform)
(require 'init-git)

;; jenkins plugin
(require 'init-jenkins)

(require 'init-whitespace)
(when *spell-check-support-enalbed*
  (require 'init-spelling))
(require 'init-shell)

(require 'init-mode-mappings)
(require 'init-xterm)
;; Diminish modeline clutter

(diminish 'wrap-region-mode)

;;Load desktop last
(require 'init-desktop)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Show matching parens when after the paren
(require 'paren) (show-paren-mode t)

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)

(provide 'init)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(fci-rule-color "#3E4451")
 '(flycheck-javascript-flow-args nil)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (flx-ido color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized move-dup powerline typescript lsp-mode lsp-ui flow-js2-mode groovy-mode dap-java dap-mode company-lsp lsp-java company-terraform diff-hl diredfl json-mode magit rjsx-mode web-mode whitespace-cleanup-mode nvm flycheck-flow ace-window avy atom-one-dark-theme abyss-theme nyan-mode feature-mode git rainbow-mode company-flow ws-butler jenkins yaml-mode terraform-mode xref-js2 tidy tagedit smex scss-mode rvm repository-root rainbow-delimiters paredit mmm-mode js-doc js-comint javadoc-lookup ido-ubiquitous helm-projectile grep-o-matic grep-a-lot flex-isearch expand-region etags-table etags-select dired-single diminish ctags auto-complete)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
