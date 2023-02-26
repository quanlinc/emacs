;;; init-git.el --- Git support
;;; Commentary:
;;; Code:

(require-package 'git-blamed)
;; (require-package 'gitignore-mode)
;; (require-package 'gitconfig-mode)
(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))


(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))


(provide 'init-git)
;;; init-git.el ends here
