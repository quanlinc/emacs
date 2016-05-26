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

(provide 'my-defuns)

