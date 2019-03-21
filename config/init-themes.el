;;; init-themes.el --- setup default theme
;;; Commentary:
;;; Code:
(require-package 'color-theme)
(require-package 'atom-one-dark-theme)

(if (display-graphic-p)
    (load-theme 'atom-one-dark t)
  (load-theme 'wheatgrass t))

(provide 'init-themes)
;;; init-themes.el ends here
