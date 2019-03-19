;;; init-tramp.el --- Tramp configuration
;;; Commentary:
;;; Code:

(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(setq tramp-default-user "root")
;;(add-to-list 'tramp-default-proxies-alist
;;             '(nil "\\`root\\'" "/ssh:%h:"))
;;(add-to-list 'tramp-default-proxies-alist
;;             '((regexp-quote (system-name)) nil nil))
;; Root access on local host: /sudo::<path-to-root-owned-file>
;; Root access on remote hosts: /sudo:root@remote-host:<path-to-root-owned-file>

(provide 'init-tramp)
;;; init-tramp.el ends here
