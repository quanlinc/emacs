;;; init-flycheck.el --- Flycheck global configuration
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (require-package 'flycheck-flow)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (after-load 'flycheck
    (diminish 'flycheck-mode " Φ"))
  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(require 'flycheck-flow)

(defun flow-type-at-pos ()
  "Show flow type at cursor."
  (interactive)
  (with-output-to-temp-buffer "*flow-type*"
    (let ((file (buffer-file-name))
          (line (line-number-at-pos))
          (col (current-column)))
      (shell-command
       (format "%s type-at-pos --from emacs %s %d %d"
               (my/get-node-modules-bin "flow")
               file
               line
               (1+ col))
       "*flow-type*" ;;stdout
       "*flow-type*") ;;stderr
      (pop-to-buffer "*flow-type*"))
    )
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
