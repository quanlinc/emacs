;;; init-mmm.el --- Multiple major mode support
;;; Commentary:
;;; Code:
(require-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffer-with-submode-classes)
(setq mmm-submode-decoration-level 2)
(provide 'init-mmm)
;;; init-mmm.el ends here
