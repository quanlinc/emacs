(require 'repository-root)
(setq repository-root-matchers (quote (repository-root-matcher/git)))
(require 'grep-o-matic)
(setq grep-o-matic-ask-about-save nil)
(setq grep-o-matic-search-patterns (quote ("*.cpp" "*.c" "*.h" "*.awk" "*.sh" "*.py"
                                           "*.pl" "[Mm]akefile" "*.el" "*handler" "*.java" "*.xml"
                                           "*.m" "*.mi" "*.rb" "*.sql" "*.js" "*.css" "*.cfg")))
(add-hook 'grep-mode-hook 'ansi-color-for-comint-mode-on)

(require 'grep-a-lot)
(grep-a-lot-setup-keys)


(setq grep-command "grep -srni ")
(setq compilation-scroll-output t)

(provide 'my-grep)
