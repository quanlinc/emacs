;;Javascript
(add-to-list 'auto-mode-alist '(".js$" . js2-mode))
(add-to-list 'auto-mode-alist '(".less$" . less-css-mode))
(add-to-list 'auto-mode-alist '(".css$" . css-mode))
(add-to-list 'auto-mode-alist '(".html$" . angular-html-mode))
;;(add-to-list 'auto-mode-alist '(".js$" . angular-mode))
(add-to-list 'auto-mode-alist '(".java$" . java-mode))
;;(add-to-list 'auto-mode-alist '(".htm$" . html-mode))
;; org mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))


(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;   To invoke html-helper-mode automatically on .html files, do this:
    ;; (setq auto-mode-alist (cons '("\\.html$" . html-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.htm$" . html-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.jsp$" . html-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.jspx$" . html-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.C$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.java$" . java-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.java.in$" . java-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.eC$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.ec$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.ecin$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.eCin$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.Cin$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.cin$" . c++-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.fin$" . fortran-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.mak$" . makefile-mode) auto-mode-alist))

(provide 'init-mode-mappings)
