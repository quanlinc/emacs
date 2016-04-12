
;; Bind the CTRL plus . "dot" key to go to next tag
(global-set-key (kbd "C-.") 'find-next-tag)

;; SET UP FUNCTION KEYS
;;   CURRENTLY USED KEYS ARE: [F1]-[F12]  ([F2] defined below)
(global-set-key [f1] 'set-mark-command)
(global-set-key [f2] 'copy-region-as-kill)
(global-set-key [f3] 'undo)
(global-set-key [f4] 'replace-string)
(global-set-key [f5] 'query-replace)
(global-set-key [f6] 'isearch-forward-regexp)

;;(global-set-key [f7] 'isearch-backward-regexp) f7 has been mapped to expand templates
(global-set-key [f8] 'replace-regexp)
(global-set-key [f9] 'goto-line)
(global-set-key [f10] 'forward-sexp)
(global-set-key [f11] 'backward-sexp)
;;(global-set-key [f12] 'delete-char)
(global-set-key [f13] 'beginning-of-buffer)
(global-set-key [f14] 'end-of-buffer)

;; Set the key bindings for the console version of emacs
(global-set-key [kp-f1] 'set-mark-command)
(global-set-key [kp-f2] 'copy-region-as-kill)
(global-set-key [kp-f3] 'undo)
(global-set-key [kp-f4] 'replace-string)
(global-set-key [kp-f5] 'query-replace)
(global-set-key [kp-f6] 'isearch-forward-regexp)
(global-set-key [kp-f8] 'replace-regexp)
(global-set-key [kp-f9] 'goto-line)
(global-set-key [kp-f10] 'forward-sexp)
(global-set-key [kp-f11] 'backward-sexp)
(global-set-key [kp-f12] 'delete-char)
(global-set-key [kp-f13] 'beginning-of-buffer)
(global-set-key [kp-f14] 'end-of-buffer)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)
;(global-set-key (kbd "C-u C-c SPC") 'ace-jump-char-mode)
;(global-set-key (kbd "C-u C-u C-c SPC") 'ace-jump-line-mode)

;; bind Alt/Meta + n/p to scroll page up/down
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))

(provide 'my-keybindings)
