;;; init-dart.el --- Dart configuration for Flutter
;;; Commentary: Package requirements and mode config, most of the setup will be overlapped in the init-lsp.el
;;; Here will just be some variable setup for Flutter and Dart to work in the dart mode
;;; Code:

;; (setq package-selected-packages
;;       `(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
;;                   ;; Optional packages
;;                   lsp-ui hover))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

;; (add-hook 'dart-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       ;; Increase the amount of data Emacs reads from the process
;;       ;; The default 4k is too low that causes certain process to crash, eslint as an example
;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
;;       )

(setq lsp-dart-sdk-dir "/Users/quanlinc/development/flutter/bin/cache/dart-sdk")
(setq lsp-dart-flutter-sdk "/Users/quanlinc/development/flutter")
(setq flutter-sdk-path "/Users/quanlinc/development/flutter")

(with-eval-after-load 'dart-mode
  (define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload))

(provide 'init-dart)
;;; init-dart.el ends here