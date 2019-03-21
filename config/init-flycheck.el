;;; init-flycheck.el --- Flycheck global configuration
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (require-package 'flycheck-flow)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;;TODO: revist this configuration

(add-hook 'clojure-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
