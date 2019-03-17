;;; init.el --- Initialization file for configuration
;;; Commentary:
;;; Code:

;; For debugging error on startup
;;(setq debug-on-error t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)

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



(defvar emacs-tern "/Users/quanlin.chen/projects/tern/emacs/"
  "tern path")

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

(add-to-list 'load-path emacs-tern)
(autoload 'tern-mode "tern.el" nil t)


(setenv "PATH" (concat (getenv "PATH") ":/Users/quanlin.chen/.nvm/versions/node/v10.7.0/bin"))
(setq exec-path (append exec-path '("/Users/quanlin.chen/.nvm/versions/node/v10.7.0/bin")))

;; (setenv "PATH" (concat (getenv "PATH") ":/Users/quanlin.chen/projects/tern/bin"))
;;     (setq exec-path (append exec-path '("/Users/quanlin.chen/projects/tern/bin")))

;; (add-to-list 'load-path "/Users/quanlin.chen/.nvm/v4.3.1/bin/")
;; (autoload 'tern-mode "tern.el" nil t)

(require 'init-benchmark)
(require 'init-defaults)
(require 'init-defuns)
(require 'init-packages)
;;(require 'init-dired)
;;(require 'dired+)
(require 'init-auto-complete)
(require 'flex-isearch)
(require 'init-mmm)
(require 'init-ui)
(require 'init-gui-frames)
(require 'init-frame-hooks)
(require 'init-defuns)
(require 'init-flycheck)
(require 'flycheck-flow)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-grep)
(require 'helm-config)
;;(require 'tern)


;; programming style
(require 'init-html)
(require 'init-javascript)
(require 'init-jsdoc)
;; jenkins plugin
(require 'init-jenkins)

(require 'init-whitespace)
(when *spell-check-support-enalbed*
  (require 'init-spelling))
(require 'init-shell)
(require 'init-themes)
;;initialize global keybindings and mode mappings
(require 'init-keybindings)
(require 'init-mode-mappings)
(require 'expand-region)
(require 'init-xterm)
;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
(diminish 'yas/minor-mode)
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

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(provide 'init)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("bf5bdab33a008333648512df0d2b9d9710bdfba12f6a768c7d2c438e1092b633" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default)))
 '(fci-rule-color "#3C3D37")
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
    (json-mode magit rjsx-mode web-mode whitespace-cleanup-mode nvm flycheck-flow ace-window avy atom-one-dark-theme abyss-theme nyan-mode feature-mode git rainbow-mode company-flow ws-butler jenkins yaml-mode terraform-mode xref-js2 tidy tagedit smex scss-mode rvm repository-root rainbow-delimiters paredit mmm-mode js2-refactor js-doc js-comint javadoc-lookup ido-ubiquitous helm-projectile grep-o-matic grep-a-lot flex-isearch expand-region etags-table etags-select dired-single diminish ctags color-theme auto-complete)))
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
