(if (fboundp 'prog-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook
                  yaml-mode-hook
                  shell-mode-hook
                  css-mode-hook
                  javascript-mode-hook))
    (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
            (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

(provide 'init-flyspell)
