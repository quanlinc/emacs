;; For debugging error on startup
;;(setq debug-on-error t)
(require 'cl)

(defvar root-dir (file-name-directory load-file-name)
  "The root dir of Emacs configuration.")

(defvar emacs-config-dir (concat root-dir "/.emacs.d/config/")
  "This directory houses all of the main configuration")
(defvar emacs-site-lisp-dir (concat root-dir "/.emacs.d/site-lisp/")
  "This directory houses other extensions and git submodules")
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
(require 'my-defuns)
(require 'my-yasnippet)
(require 'my-projectile)
(require 'my-grep)
(require 'my-javascript)
(require 'my-jsdoc)
(require 'my-flycheck)
(require 'my-shell)
(require 'my-themes)
;;initialize global keybindings and mode mappings
(require 'my-keybindings)
(require 'my-mode-mappings)
(require 'expand-region)
;;Load desktop last
(require 'my-desktop)
;; Show matching parens when after the paren
(require 'paren) (show-paren-mode t)
