;;; init-uniquify.el --- uniquification of buffer name
;;; Commentary:
;;; Code:

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(provide 'init-uniquify)
;;; init-uniquify.el ends here
