;;; init-lsp.el --- Language Server Protocol
;;; Commentary: Package requirements and mode config
;;; Code:

(maybe-require-package 'lsp-mode)
(maybe-require-package 'lsp-java)
(maybe-require-package 'lsp-ui)
(maybe-require-package 'company-lsp)
(maybe-require-package 'lsp-treemacs)
(maybe-require-package 'helm-lsp)

(require 'lsp-mode)
(require 'lsp-clients)

(add-hook 'js2-mode-hook 'lsp)

;;(add-hook 'typescript-mode-hook, #'lsp)

(push 'company-lsp company-backends)

;; Disable client-side cache because the LSP server does a better job
(setq company-transformers nil
      company-lsp-async t
      company-lsp-cache-candidates nil)

(provide 'init-lsp)
;;; init-lsp.el ends here