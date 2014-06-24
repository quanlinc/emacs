(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-quick-help t)
(setq ac-use-menu-map t)
;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(provide 'my-auto-complete)
