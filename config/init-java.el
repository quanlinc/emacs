;;; init-java.el --- Java  development suport
;;; Commentary:
;;; Code:

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(require-package 'dap-mode)
(after-load 'dap-mode
  (require 'dap-java)
  (dap-mode 1)
  (dap-ui-mode 1))

(setenv "JAVA_HOME"  "/opt/homebrew/Cellar/openjdk@11/11.0.18/libexec/openjdk.jdk/Contents/Home/")
(setq lsp-java-java-path "/opt/homebrew/Cellar/openjdk@11/11.0.18/libexec/openjdk.jdk/Contents/Home/bin/java")


(setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
                                               :path "/Library/Java/JavaVirtualMachines/jdk1.8.0_101.jdk/Contents/Home/")
                                        (:name "JavaSE-11"
                                               :path "/opt/homebrew/Cellar/openjdk@11/11.0.18/libexec/openjdk.jdk/Contents/Home/"
                                               :default t)])

(setq lsp-java-vmargs
      '(
        "-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        "-javaagent:/Users/tranchen/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))

(dap-register-debug-template "My Runner"
                             (list :type "java"
                                   :request "launch"
                                   :args ""
                                   :vmArgs "-ea -Dmyapp.instance.name=myapp_1"
                                   :projectName "demo"
                                   :mainClass "com.example.demo.DemoApplication"
                                   :env '(("DEV" . "1"))))

;; Fix compile escape codes
(add-hook 'compilation-filter-hook
          (lambda ()  (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))


(provide 'init-java)
;;; init-java.el ends here
