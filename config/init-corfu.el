;;; init-corfu.el --- Interactive completion in buffers
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(require-package 'orderless)
(with-eval-after-load 'vertico
  (require 'orderless))
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides nil)
(setq completino-cycle-threshold 4)

(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)

  (when (maybe-require-package 'corfu-doc)
    (with-eval-after-load 'corfu
      (add-hook 'corfu-mode-hook #'corfu-doc-mode)))
  ;;TODO: kind-icon
  (when (maybe-require-package 'kind-icon)
    (with-eval-after-load 'corfu
      (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

      (kind-icon-default-face 'corfu-default)))
  )


(provide 'init-corfu)
;;; init-corfu.el ends here
