;;; init-projectile.el --- Project navigation using projectile
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)
  (require-package 'helm)
  (require-package 'helm-projectile)

  ;; Shorter modeline
  ;;(setq-default projectile-mode-line-prefix " Proj")

  ;; set the path for where projects live
  (setq projectile-project-search-path '("/Volumes/Datastore/projects"))
  (after-load 'projectile
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

    (diminish 'projectile-mode)

    (setq projectile-completion-system 'helm)
    (helm-projectile-on)

    (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
    (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
    ))

(provide 'init-projectile)
;;; init-projectile.el ends here
