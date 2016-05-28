
;; Bind the CTRL plus . "dot" key to go to next tag
(global-set-key (kbd "C-.") 'find-next-tag)

(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)
;(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)
;(global-set-key (kbd "C-u C-c SPC") 'ace-jump-char-mode)
;(global-set-key (kbd "C-u C-u C-c SPC") 'ace-jump-line-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
;; bind Alt/Meta + n/p to scroll page up/down
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))
;; keybinding for expand region
(global-set-key (kbd "C-=") 'er/expand-region)
;; console mode emacs has an issue with this keybinding

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'init-keybindings)