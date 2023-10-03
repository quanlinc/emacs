;;; init-line-model.el --- use telephone line mode
;;; Commentary:
;;; Code:
(require-package 'telephone-line)

(setq telephone-line-primary-left-separator 'telephone-line-tan-left
      telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 20
      telephone-line-evil-use-short-tag t)

(telephone-line-mode 1)

(provide 'init-line-mode)