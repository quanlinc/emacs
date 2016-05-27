;;; Initialization file for configuration

;; For debugging error on startup
;;(setq debug-on-error t)
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

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)

(require 'init-benchmark)
(require 'init-defaults)
(require 'init-defuns)
(require 'init-packages)
(require 'init-dired)
(require 'init-auto-complete)
(require 'flex-isearch)
(require 'init-mmm)
(require 'init-ui)
(require 'init-gui-frames)
(require 'init-defuns)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-grep)

;; programming style
(require 'init-html)
(require 'init-javascript)
(require 'init-jsdoc)

(require 'init-flycheck)
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
;;Load desktop last
(require 'init-desktop)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
;; Show matching parens when after the paren
(require 'paren) (show-paren-mode t)


(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)

;;; init.el ends here
