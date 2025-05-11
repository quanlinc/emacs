;;; init-lsp.el --- Language Server Protocol
;;; Commentary: Package requirements and mode config
;;; Code:

(require-package 'lsp-mode)
(require-package 'lsp-java)
(require-package 'lsp-ui)
(require-package 'lsp-treemacs)
(require-package 'helm-lsp)
(require-package 'lsp-dart)

(require 'lsp-mode)
(require 'lsp-ui)

(after-load 'lsp-mode
  (diminish 'lsp-mode))

(add-hook 'js2-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'typescript-mode-hook 'flycheck-mode)
(add-hook 'dart-mode-hook 'lsp)

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

;; Increase the amount of data Emacs reads from the process
;; The default 4k is too low that causes certain process to crash, eslint as an example
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold (* 100 1024 1024))

(setq lsp-eslint-enable 0)

(provide 'init-lsp)
;;; init-lsp.el ends here