;;; init-yaml.el --- Support Yaml files
;;; Commentary:
;;; Code:

(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))


(provide 'init-yaml)
;;; init-yaml.el ends here