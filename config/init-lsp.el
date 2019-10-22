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

(after-load 'lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top-edge
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here