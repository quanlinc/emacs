;; Shell stuff
(setq binary-process-input t) 
(setq w32-quote-process-args ?\") 
(setq shell-file-name "zsh")
(setenv "SHELL" shell-file-name) 
(setq explicit-shell-file-name shell-file-name) 
(setq explicit-sh-args '("-login" "-i"))


;; Emacs shell working directory tracking
  (defun track-shell-directory/procfs ()
    (shell-dirtrack-mode 0)
    (add-hook 'comint-preoutput-filter-functions
              (lambda (str)
                (prog1 str
                  (when (string-match comint-prompt-regexp str)
                    (cd (file-symlink-p
                         (format "/proc/%s/cwd" (process-id
                                                 (get-buffer-process
                                                  (current-buffer)))))))))
              nil t))

  (add-hook 'shell-mode-hook 'track-shell-directory/procfs)

(defun add-mode-line-dirtrack ()
  (add-to-list 'mode-line-buffer-identification
               '(:propertize (" " default-directory " ") face dired-directory)))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)
(add-hook 'eshell-mode-hook 'add-mode-line-dirtrack)
(setq eshell-prompt-function (lambda nil (concat (if (= (user-uid) 0) " # " "$ "))))

;; function to start a shell in a given dir, and rename the shell
(defun init-shell (starting-dir name)
  (cd starting-dir)
  (shell)
  (rename-buffer name))

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun shell-clear ()
  (interactive)
  (erase-buffer)
  (comint-send-input))

(provide 'my-shell)
