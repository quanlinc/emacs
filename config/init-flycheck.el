(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;;TODO: revist this configuration
;;(require 'flycheck)
(add-hook 'clojure-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))

(provide 'init-flycheck)
