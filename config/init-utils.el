;;; init-utils.el --- Helper functions
;;; Commentary:
;;; Code:

(defun indent-block (from to)
  (interactive "r")
  (indent-rigidly from to tab-width))

(defun unindent-block (from to)
  (interactive "r")
  (indent-rigidly from to (- tab-width)))

;; Make it easy to jump to next tag by hitting CTRL+.
(defun find-next-tag ()
  (interactive)
  (find-tag "" t))

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

(defun my/find-javascript-flycheck-backends ()
  (message "Setting up find hooks for Javascript Flycheck backends")
  (add-hook 'flycheck-mode-hook
            (apply-partially #'my/use-checker-from-node-modules "flow" "flow"))
  (add-hook 'flycheck-mode-hook
            (apply-partially #'my/use-checker-from-node-modules "eslint"
                             "eslint"))
  (add-hook 'flycheck-mode-hook
            (apply-partially #'my/use-checker-from-node-modules "jshint"
                             "jshint"))
  (add-hook 'flycheck-mode-hook
            (apply-partially #'my/use-checker-from-node-modules
                             "flow-coverage"
                             "flow"))
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
  ;; (setq-default company-backends
  ;;   ;; tern works its way in here multiple times.
  ;;   (remove 'company-tern (remove-duplicates company-backends))
  ;; )
  ;; (setq-default company-backends-rjsx-mode
  ;;   ;; tern works its way in here multiple times.
  ;;   (remove 'company-tern (remove-duplicates company-backends-rjsx-mode))
  ;; )
  ;; (setq-default company-backends-rjsx-mode-raw
  ;;   ;; tern works its way in here multiple times.
  ;;   (remove 'company-tern (remove-duplicates company-backends-rjsx-mode-raw))
  ;; )
  (add-to-list 'company-backends 'company-flow)
  ;; (add-to-list 'company-backends-rjsx-mode 'company-flow)

  (message "company backends for JS %s" company-backends)
  ;; (message "company backends for JSX %s" company-backends-rjsx-mode)

  ;; NOTE: We also need to configure flow's backend on a per major-mode basis.
  ;; (setq-default company-backends-js2-mode
  ;;   ;; tern works its way in here multiple times.
  ;;   (remove 'company-tern (remove-duplicates company-backends-js2-mode))
  ;; )
  ;; (add-to-list 'company-backends-js2-mode 'company-flow)
  ;; (message "company backends (js2mode) (after JS config): %s" company-backends-js2-mode)
)

(defun flow-type-at-pos ()
  "Show flow type at the cursor."
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos))
        (col (current-column))
        (buffer (current-buffer)))
    (display-buffer (get-buffer-create "*Shell Command Output*")
      '((
          display-buffer-reuse-window
          display-buffer-pop-up-window
          display-buffer-pop-up-frame
        )
        (reusable-frames . 0))
      )
    (switch-to-buffer-other-window "*Shell Command Output*")
    (shell-command
      (format "%s type-at-pos --from emacs %s %d %d"
              (my/get-node-modules-bin "flow")
              file
              line
              (1+ col)))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
  )

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


(provide 'init-utils)
;;; init-utils.el ends here
