
;; Bind the CTRL plus . "dot" key to go to next tag
(global-set-key (kbd "C-.") 'find-next-tag)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)

;;Helm
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; bind Alt/Meta + n/p to scroll page up/down
(global-set-key "\M-n" (lambda() (interactive) (scroll-up 1)))
(global-set-key "\M-p" (lambda() (interactive) (scroll-down 1)))
;; keybinding for expand region
(global-set-key (kbd "C-=") 'er/expand-region)
;; console mode emacs has an issue with this keybinding

(global-set-key (kbd "C-x g") 'magit-status)

;; Show-hide
(global-set-key (kbd "C-c C-n") 'hs-show-block)
;;(global-set-key (kbd "") 'hs-show-all)
(global-set-key (kbd "C-c C-h") 'hs-hide-block)
;;(global-set-key (kbd "") 'hs-hide-all)

(provide 'init-keybindings)
