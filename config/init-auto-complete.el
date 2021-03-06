;;; init-auto-complete.el --- Setting for auto completion
;;; Commentary:
;;; Code:

(require-package 'auto-complete)
(require-package 'auto-complete-config)

(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")
(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
