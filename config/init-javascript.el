;;; init-javascript.el --- Javascript support
;;; Commentary:
;;; Code:

(maybe-require-package 'json-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'js2-refactor)
(maybe-require-package 'nvm)
(maybe-require-package 'typescript-mode)
(maybe-require-package 'prettier-js)

(setq-default flycheck-javascript-flow-args '("--respect-pragma"))

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(define-key js-mode-map (kbd "M-.") nil)

(autoload 'js2-mode "js2-mode" nil t)

;; js2-mode
;; Change some defaults: customize them to override
(setq-default js2-bounce-indent-p nil)
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
      (setq-local js2-mode-show-parse-errors t)
      (setq-local js2-mode-show-strict-warnings t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (add-hook 'js2-jsx-mode-hook (lambda () (setq mode-name "JSX2")))
  (js2-imenu-extras-setup))

;; prevent indentation from lining up with a prior line's glyph
;; this will make it so fighting is less necessary to appease linters
(setq-default js2-pretty-multiline-declarations nil)

;; default indentation
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

;; In Emacs >= 25, the following is an alias for js-indent-level
(setq-default js2-basic-offset 2)

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))


;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'js-comint)
  (setq js-comint-program-command "node")
  (setq js-comint-program-arguments '("--interactive"))
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    :init-value nil :lighter " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

(defun inferior-js-mode-hook-setup ()
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

(when (maybe-require-package 'add-node-modules-path)
  (dolist (mode '(typescript-mode js-mode js2-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)
    (add-hook (derived-mode-hook-name mode) 'prettier-js)
    (add-hook (derived-mode-hook-name mode) 'eglot-ensure)))

(provide 'init-javascript)
;;; init-javascript.el ends here
