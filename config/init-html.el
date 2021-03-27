;;; init-html.el --- Editing HTML
;;; Commentary:
;;; Code:

(require-package 'tagedit)
(with-eval-after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (define-key tagedit-mode-map (kbd "M-s") nil)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

(provide 'init-html)
;;; init-html.el ends here
