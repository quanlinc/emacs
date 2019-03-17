;;; init-themes.el --- setup default theme
;;; Commentary:
;;; Code:
(if (display-graphic-p)
    (load-theme 'atom-one-dark t)
  (load-theme 'wheatgrass t))

(provide 'init-themes)
;;; init-themes.el ends here
