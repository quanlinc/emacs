;;; init-java.el --- Java  development suport
;;; Commentary:
;;; Code:

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(require-package 'dap-mode)
(after-load 'dap-mode
  (require 'dap-java)
  (dap-mode 1)
  (dap-ui-mode 1))

(setq lsp-java-vmargs
      '(
        "-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        "-javaagent:/Users/tranchen/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))

;; (after-load 'lsp-java
;;   (add-hook 'java-mode-hook #'lsp)
;;   (require 'company-lsp)
;;   (push 'company-lsp company-backends)
;;   (require-package 'dap-mode))

(provide 'init-java)
;;; init-java.el ends here
