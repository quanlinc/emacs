(require 'flycheck)
(add-hook 'clojure-mode-hook
          (lambda () (flycheck-mode t)))
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))
(provide 'my-flycheck)
