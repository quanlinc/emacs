;;; init-yasnippet.el --- Support for code snippet
;;; Commentary:
;;; Code:
(require 'yasnippet)
(after-load 'yasnippet
  (diminish 'yas-minor-mode))
;; Allow nested expansions
(setq yas-triggers-in-field t)

;; Prompt with an ido-style prompt for keys with multiple expansions
(setq yas-prompt-functions '(yas-ido-prompt))

;; Prevent from hi-jacking the tab key
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
;; Setup my snippets directory
(setq yas-snippet-dirs (list
                        emacs-snippet-dir))
(yas-reload-all)
(yas-global-mode t)

(add-hook 'yas-after-exit-snippet-hook 'indent-for-tab-command)

(defun snippet-mode-setup ()
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil))

(add-hook 'snippet-mode-hook 'snippet-mode-setup)
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
