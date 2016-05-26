(require 'projectile)
(projectile-global-mode t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
(add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html

(provide 'my-projectile)
