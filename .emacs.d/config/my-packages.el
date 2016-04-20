(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
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

(defvar required-packages
  '(auto-complete
    color-theme
    ctags
    dash
    dired-single
    etags-select
    etags-table
    ido-ubiquitous
    javadoc-lookup
    js-comint
    js2-mode
    paredit
    rvm
    s
    scss-mode
    smex
    yasnippet
    zenburn-theme
    )
  "A list of packages are ensured to be installed at launch.")

(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'my-packages)
