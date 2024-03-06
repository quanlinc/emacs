;;; init.el --- Initialization file for configuration
;;; Commentary:
;;; Code:

;; For debugging error on startup
;; (setq debug-on-error t)

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "27.1")
  (message "Your Emacs is old, some functionality in this config will be disabled. Please upgrade Emacs."))

;; toggle the default spelling check feature
(defconst *spell-check-support-enalbed* t)
(defconst *is-a-mac* (eq system-type 'darwin))

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")
(defvar emacs-config-dir (concat root-dir "config/")
  "This directory houses all of the main configuration")
(defvar emacs-site-lisp-dir (concat root-dir "site-lisp/")
  "This directory houses other extensions and git submodules")
(defvar emacs-snippet-dir (concat root-dir "snippets/")
  "This directory contains all snippets for yasnippet")


(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

;; known bug to MacOS Ventura, will be fixed in emacs 29.x
;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
(add-to-list 'image-types 'svg)

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
(require 'init-exec-path)
(require 'init-utils)
(require-package 'diminish)
(require 'init-editing-utils)
(require 'init-line-mode)
(require 'init-dash)
(require 'init-dired)
(require 'flex-isearch)
(require 'init-company)
;;(require 'init-corfu)
(require 'init-mmm)
(require 'init-nix)
(require 'init-gui-frames)
(require 'init-themes)
(require 'init-frame-hooks)
(require 'init-uniquify)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-grep)
(require 'init-tramp)
(require 'init-flycheck)
(require 'init-lsp)
;;(require 'init-eglot)
(require 'prisma-mode)

;; programming style
(require 'init-lisp)
(require 'init-html)
(require 'init-css)
(require 'init-csv)
(require 'init-java)
(require 'init-javascript)
(require 'init-jsdoc)
(require 'init-python)
(require 'init-terraform)
(require 'init-git)
(require 'init-yaml)
(require 'init-toml)
(require 'init-rust)
(require 'init-docker)

;; org mode
(require 'init-org)

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
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(lsp-java-java-path
   "/Users/tranchen/Library/Java/JavaVirtualMachines/corretto-17.0.7/Contents/Home/bin/java")
 '(package-selected-packages
   '(telephone-line company-tabnine helm-lsp lsp-ui lsp-java lsp-mode yasnippet-snippets yasnippet-classic-snippets yaml ws-butler writeroom-mode whitespace-cleanup-mode which-key wgrep web-mode vterm vlf typescript-mode treemacs toml-mode terraform-mode tagedit switch-window spinner skewer-less sass-mode rust-mode rjsx-mode rainbow-mode rainbow-delimiters prettier-js pip-requirements paredit page-break-lines origami org-pomodoro org-cliplink orderless nvm ns-auto-titlebar nixpkgs-fmt nix-sandbox nix-mode nix-buffer move-dup mmm-mode markdown-mode magit macrostep kind-icon json-mode js2-refactor js-doc js-comint ipretty immortal-scratch ido-completing-read+ highlight-quoted helm-projectile grab-mac-link goto-line-preview git-timemachine git-blamed fullframe flycheck-rust flycheck-package flycheck-flow flycheck-color-mode-line flex-isearch expand-region exec-path-from-shell elisp-slime-nav eglot-java dockerfile-mode docker-compose-mode docker diredfl dired-single dimmer diminish diff-hl default-text-scale dash-at-point csv-mode css-eldoc corfu consult-eglot color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-modern cl-libify cask-mode beacon auto-compile aggressive-indent add-node-modules-path))
 '(warning-suppress-log-types '(((tar link)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
