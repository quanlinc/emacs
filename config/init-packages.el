;;; init-packages.el --- Packages installation and repository setup
;;; Commentary:
;;; Code:

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

;;TODO: slowly reduce the size of the required package list and move towards loading packages on demand using require-package/maybe-require-package
(defvar required-packages
  '(
    avy
    auto-complete
    ;;TODO figure out how to load theme as it's causing package.el to add trailing dash
    ;;atom-one-dark
    color-theme
    company-flow
    ctags
    dash
    diminish
    dired-single
    etags-select
    etags-table
    expand-region
    flycheck
    flycheck-flow
    grep-o-matic
    helm
    helm-projectile
    ido-ubiquitous
    javadoc-lookup
    js-comint
    js2-mode
    js2-refactor
    json-mode
    magit
    meghanada
    mmm-mode
    paredit
    projectile
    rvm
    nvm
    rjsx-mode
    s
    scss-mode
    smex
    web-mode
    ws-butler
    xref-js2
    yasnippet
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

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

(provide 'init-packages)
;;; init-packages.el ends here
