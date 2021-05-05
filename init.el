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
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

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