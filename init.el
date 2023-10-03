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
(require 'init-eglot)
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