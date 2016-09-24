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



(defvar emacs-tern "/Users/quanlin.chen/projects/tern/emacs/";(concat root-dir "/elpa/tern-20160817.522/")
  "tern path")

(add-to-list 'load-path emacs-config-dir)
(add-to-list 'load-path emacs-site-lisp-dir)





(add-to-list 'load-path emacs-tern)
(autoload 'tern-mode "tern.el" nil t)


(setenv "PATH" (concat (getenv "PATH") ":/Users/quanlin.chen/.nvm/v4.3.1/bin"))
(setq exec-path (append exec-path '("/Users/quanlin.chen/.nvm/v4.3.1/bin")))

;; (setenv "PATH" (concat (getenv "PATH") ":/Users/quanlin.chen/projects/tern/bin"))
;;     (setq exec-path (append exec-path '("/Users/quanlin.chen/projects/tern/bin")))

;; (add-to-list 'load-path "/Users/quanlin.chen/.nvm/v4.3.1/bin/")
;; (autoload 'tern-mode "tern.el" nil t)



(require 'init-benchmark)
(require 'init-defaults)
(require 'init-defuns)
(require 'init-packages)
(require 'init-dired)
(require 'dired+)
(require 'init-auto-complete)
(require 'flex-isearch)
(require 'init-mmm)
(require 'init-ui)
(require 'init-gui-frames)
(require 'init-defuns)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-grep)

;(require 'tern)

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
;; Diminish modeline clutter
(require 'diminish)
(diminish 'wrap-region-mode)
(diminish 'yas/minor-mode)
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
