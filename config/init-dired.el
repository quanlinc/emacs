;;; init-dired.el --- Dired customizations
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (with-eval-after-load 'dired
    (diredfl-global-mode)))

(with-eval-after-load 'dired
  (when (maybe-require-package 'dired-single)
    (autoload 'dired-single-buffer "dired-single" "" t)
    (autoload 'dired-single-buffer-mouse "dired-single" "" t)
    (autoload 'dired-single-magic-buffer "dired-single" "" t)
    (autoload 'dired-single-toggle-buffer-name "dired-single" "" t))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(provide 'init-dired)
;;; init-dired.el ends here
