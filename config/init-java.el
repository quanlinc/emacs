;;; init-java.el --- Java  development suport
;;; Commentary:
;;; Code:

(require-package 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(require-package 'dap-mode)
(after-load 'dap-mode
  (require 'dap-java)
  (dap-mode 1)
  (dap-ui-mode 1))

;; (after-load 'lsp-java
;;   (add-hook 'java-mode-hook #'lsp)
;;   (require 'company-lsp)
;;   (push 'company-lsp company-backends)
;;   (require-package 'dap-mode))

(provide 'init-java)
;;; init-java.el ends here
