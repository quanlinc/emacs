;;putting both c and java stuff here
(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
				      (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
				      (inher-intro)
				         (case-label after)
					    (label after)
					       (access-label after)))
    (c-cleanup-list             . (scope-operator
				      empty-defun-braces
				         defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				      (substatement-open . 0)
				         (case-label        . 4)
					    (inline-open       . 0)
					       (block-open        . 0)
					          (statement-cont    . ++)
						     (knr-argdecl-intro . -)))
;;    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  (setq c-basic-offset 4)  
  (c-set-offset 'substatement-open 0)
  ;; offset customizations not in my-c-style
  (c-set-offset 'member-init-intro 0)

  ;; other customizations
  (setq tab-width 2
	;; this will make sure spaces are used instead of tabs
	indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
;;  (c-toggle-auto-hungry-state 0)

  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  )
     
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



;;; Make sure the JDE is in the load path
;;/usr/jde/lisp"
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/speedbar-0.13a"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/semantic-1.3.3"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/semantic-1.4"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/elib-1.0"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/eieio-0.16"))
;(add-to-list 'load-path (expand-file-name "/emacs-21.2/jde-2.2.8/lisp/eieio-0.17beta4"))

;;; Require the JDE...


;; If you want Emacs to defer loading the JDE until you open a 
;; Java file, edit the following line
(setq defer-loading-jde nil)
;; to read:
;;
  (setq defer-loading-jde t)
;;

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	        (append
		      '(("\\.java\\'" . jde-mode))
		           auto-mode-alist)))
  (require 'jde))



;; Sets the basic indentation for Java source files
;; to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 2))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)


(provide 'init-java)
