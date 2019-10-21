;;; init-flycheck.el --- Flycheck global configuration
;;; Commentary:
;;; Code:

(when (maybe-require-package 'flycheck)
  (require-package 'flycheck-flow)
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (when (maybe-require-package 'flycheck-color-mode-line)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(require 'flycheck-flow)

;;javascript development related function definitions
(defun my/get-node-modules-bin (bin-name)
  "Find BIN-NAME inside of =./node_modules/.bin/= ."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules")
               ))
    (expand-file-name (concat "node_modules/.bin/" bin-name) root)
    )
  )

(defun my/use-checker-from-node-modules (checker-name bin-name)
  "For a given CHECKER-NAME, find the BIN-NAME inside of node_modules."
  (let* ((path (my/get-node-modules-bin bin-name)))
    (if path
        (let ((checker-exec-sym (intern (concat "flycheck-javascript-" checker-name "-executable"))))
          (make-local-variable checker-exec-sym)
          (set checker-exec-sym path)
          (add-to-list 'tramp-remote-path (file-name-directory path))
          (message "using flycheck checker %s" path)
          )
      (message "flycheck -- checker %s not available for mode %s with file %s"
               checker-name major-mode buffer-file-name)
      )
    )
  )

(defun my/find-javascript-flycheck-backends (hook)
  (message "Setting up find hooks for Javascript Flycheck backends")
  (add-hook hook
            (apply-partially #'my/use-checker-from-node-modules "flow" "flow"))
  (add-hook hook
            (apply-partially #'my/use-checker-from-node-modules "eslint" "eslint"))
  (add-hook hook
            (apply-partially #'my/use-checker-from-node-modules "jshint" "jshint"))
  (add-hook hook
            (apply-partially #'my/use-checker-from-node-modules "flow-coverage" "flow"))
  )

(defun my/connect-javascript-flycheck-backends ()
  (message "Connecting Javascript Flycheck backends")
  (flycheck-add-next-checker 'javascript-flow 'javascript-flow-coverage)
  (flycheck-add-next-checker 'javascript-flow-coverage 'javascript-eslint)
  )

(defun my/use-company-backend-from-node-modules (bin-name)
  "For a given CHECKER-NAME, find the BIN-NAME inside of node_modules."
  (let* ((path (my/get-node-modules-bin bin-name)))
    (if path
        (let ((backend-exec-sym (intern
                                 (concat "company-" bin-name "-executable"))))
          (make-local-variable backend-exec-sym)
          (set backend-exec-sym path)
          )
      (message
       "company-mode -- backend %s not available for mode %s with file %s"
       bin-name major-mode buffer-file-name)
      )
    )
  )

(defun my/find-javascript-company-backends ()
  (message "Setting up find hooks for Javascript Company backends")
  (add-hook 'company-mode-hook
    (apply-partially #'my/use-company-backend-from-node-modules "flow"))
  )

(defun my/config-javascript-company-backends ()
  (require 'company-flow)
  (my/find-javascript-company-backends)
  (add-to-list 'company-backends 'company-flow)

  (message "company backends for JS %s" company-backends)
  ;; (message "company backends for JSX %s" company-backends-rjsx-mode)

  ;; NOTE: We also need to configure flow's backend on a per major-mode basis.
  ;; (add-to-list 'company-backends-js2-mode 'company-flow)
  ;; (message "company backends (js2mode) (after JS config): %s" company-backends-js2-mode)
  )

(defun flow-type-at-pos ()
  "Show flow type at cursor."
  (interactive)
  (with-output-to-temp-buffer "*flow-type*"
    (let ((file (buffer-file-name))
          (line (line-number-at-pos))
          (col (current-column)))
      (shell-command
       (format "%s type-at-pos --from emacs %s %d %d"
               (my/get-node-modules-bin "flow")
               file
               line
               (1+ col))
       "*flow-type*" ;;stdout
       "*flow-type*") ;;stderr
      (pop-to-buffer "*flow-type*"))
    )
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
