;;; init-company.el --- company mode setting
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(when (maybe-require-package 'company)
  (require 'company-tabnine)
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (dolist (backend '(company-eclim company-semantic))
      (delq backend company-backends))
    (diminish 'company-mode)
    ;; disable following two lines of keybinding due to not working for camel case
    ;; (define-key company-mode-map (kbd "M-/") 'company-complete)
    ;; (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t))
  (global-set-key (kbd "M-C-/") 'company-complete)
  (when (maybe-require-package 'company-quickhelp)
    (add-hook 'after-init-hook 'company-quickhelp-mode)))

;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(with-eval-after-load 'company
  (with-eval-after-load 'page-break-lines
    (defvar-local sanityinc/page-break-lines-on-p nil)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'sanityinc/page-break-lines-maybe-reenable))
  (add-to-list 'company-backends #'company-tabnine)
  )

(when (maybe-require-package 'vertico)
  (with-eval-after-load 'vertico
    (require 'orderless))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil)
  (setq completion-cycle-threshold 4))


(provide 'init-company)
;;; init-company.el ends here