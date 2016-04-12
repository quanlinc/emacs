(require 'projectile)
(projectile-global-mode t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'my-projectile)
