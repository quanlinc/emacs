;;; init-java.el --- Java  development support
;;; Commentary:
;;; Code:

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
;; TODO: figure out how to get lombok work with eglot
;; (require-package 'eglot-java)
;; (add-hook 'java-mode-hook 'eglot-java-mode)

;; (defconst my-eclipse-jdt-home "/Users/tranchen/.emacs.d/.cache/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.400.v20210924-0641.jar")
;; (defun my-eglot-eclipse-jdt-contact (interactive)
;;   "Contact with the jdt server input INTERACTIVE."
;;   (let ((cp (getenv "CLASSPATH")))
;;     (setenv "CLASSPATH" (concat cp ":" my-eclipse-jdt-home))
;;     (unwind-protect (eglot--eclipse-jdt-contact nil)
;;       (setenv "CLASSPATH" cp))))

;; (setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)
;; (add-hook 'java-mode-hook 'eglot-ensure)

;; (defvar +eglot/initialization-options-map (make-hash-table :size 5))

;; (cl-defmethod eglot-initialization-options ((server eglot-lsp-server))
;;   (if-let ((init-options (gethash (eglot--major-mode server) +eglot/initialization-options-map)))
;;       init-options
;;     eglot--{}))

;; (add-to-list 'eglot-server-programs
;;              `(java-mode "jdtls"
;;                          "-configuration" ,(expand-file-name "cache/language-server/java/jdtls/config_linux" user-emacs-directory)
;;                          "-data" ,(expand-file-name "cache/java-workspace" user-emacs-directory)
;;                          ,(concat "--jvm-arg=-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))))



;; TODO doesn't look like eglot supports dap mode, and eglot java doesn't seem to work with lombok very well
(require-package 'dap-mode)

(after-load 'dap-mode
  (require 'dap-java)
  (dap-mode 1)
  (dap-ui-mode 1))
;;(setenv "JAVA_HOME" "/Users/tranchen/Library/Java/JavaVirtualMachines/corretto-17.0.7/Contents/Home/")
(setenv "JAVA_HOME"  "/opt/homebrew/Cellar/openjdk@11/11.0.18/libexec/openjdk.jdk/Contents/Home/")


;; (setq lsp-java-java-path "/opt/homebrew/Cellar/openjdk@11/11.0.18/libexec/openjdk.jdk/Contents/Home/bin/java")


(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                               :path "/Users/tranchen/Library/Java/JavaVirtualMachines/corretto-1.8.0_372/Contents/Home/"
                                               ;;:path "/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/"
                                               )
                                        (:name "JavaSE-11"
                                               :path "/opt/homebrew/Cellar/openjdk@11/11.0.19/libexec/openjdk.jdk/Contents/Home/")
                                        (:name "JavaSE-17"
                                               :path "/Users/tranchen/Library/Java/JavaVirtualMachines/corretto-17.0.7/Contents/Home/"
                                               :default t)])
(setq lsp-java-vmargs
      '(
        "-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        "-javaagent:/Users/tranchen/.m2/repository/org/projectlombok/lombok/1.18.20/lnnombok-1.18.20.jar"))

;; (setq eglot-java-eclipse-jdt-args
;;       '(
;;         "-noverify"
;;         "-Xmx1G"
;;         "-XX:+UseG1GC"
;;         "-XX:+UseStringDeduplication"
;;         "-javaagent:/Users/tranchen/.m2/repository/org/projectlombok/lombok/1.18.20.jar"
;;         "-Xbootclasspath/a:/Users/tranchen/.m2/repository/org/projectlombok/lombok/1.18.20.jar"
;;         ))

;; Fix compile escape codes
(add-hook 'compilation-filter-hook
          (lambda ()  (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))


(provide 'init-java)
;;; init-java.el ends here
