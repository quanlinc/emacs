;;; init-javascript.el --- Javascript support
;;; Commentary:
;;; Code:

(require 'json-mode)
(require 'js2-refactor)
(require 'xref-js2)
(require 'nvm)

(setq-default flycheck-javascript-flow-args '("--respect-pragma"))
(nvm-use "10.7.0")
(add-to-list 'grep-find-ignored-directories "node_modules")

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)


(define-key js-mode-map (kbd "M-.") nil)

(autoload 'js2-mode "js2-mode" nil t)

(after-load 'js2-mode

  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-trailing-comma-warning nil
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  ;; other customizations
  (setq tab-width 2
	;; this will make sure spaces are used instead of tabs
	indent-tabs-mode nil)
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
;;  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

;; Javascript nests {} and () a lot, so I find this helpful

(require-package 'rainbow-delimiters)
(dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
  (add-hook hook 'rainbow-delimiters-mode))

;; prevent indentation from lining up with a prior line's glyph
;; this will make it so fighting is less necessary to appease linters
(setq-default js2-pretty-multiline-declarations nil)
(my/find-javascript-flycheck-backends)
(my/connect-javascript-flycheck-backends)
(my/config-javascript-company-backends)

(setq js-indent-level 2)


(require 'js-comint)

(setq inferior-js-program-command "/Users/quanlin.chen/.nvm/versions/node/v10.7.0/bin/node")

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
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(provide 'init-javascript)
;;; init-javascript.el ends here
