(setq js-indent-level 4)
(autoload 'js2-mode "js2-mode" nil t)

;(add-hook 'js2-mode-hook 'mbeenen-prog-mode-hook)
;(add-hook 'js-mode-hook 'mbeenen-prog-mode-hook)

(require 'js-comint)

(setq inferior-js-program-command "nodejs")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    ;; (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(provide 'my-javascript)
