;;; init-java.el --- Java  development suport
;;; Commentary:
;;; Code:

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
;; (setq defer-loading-jde nil)
;; ;; to read:
;; ;;
;;   (setq defer-loading-jde t)
;; ;;

;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;; 	        (append
;; 		      '(("\\.java\\'" . jde-mode))
;; 		           auto-mode-alist)))
;;   (require 'jde))



;; ;; Sets the basic indentation for Java source files
;; ;; to two spaces.
;; (defun my-jde-mode-hook ()
;;   (setq c-basic-offset 2))

;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))


(provide 'init-java)
;;; init-java.el ends here
