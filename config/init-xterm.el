(require 'init-frame-hooks)

(defun fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(defun sanityinc/console-frame-setup ()
  (when (< emacs-major-version 23)
    (fix-up-xterm-control-arrows))
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (when (fboundp 'mwheel-install)
    (mwheel-install)))

;; for console version of emacs, enable mouse scrolling
;; (xterm-mouse-mode t)
;; (setq mouse-wheel-follow-mouse 't)


;; (defvar alternating-scroll-down-next t)
;; (defvar alternating-scroll-up-next t)

;; (defun alternating-scroll-down-line ()
;;   (interactive "@")
;;   (when alternating-scroll-down-next
;;                                         ;      (run-hook-with-args 'window-scroll-functions )
;;     (scroll-down-line))
;;   (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

;; (defun alternating-scroll-up-line ()
;;   (interactive "@")
;;   (when alternating-scroll-up-next
;;                                         ;      (run-hook-with-args 'window-scroll-functions)
;;     (scroll-up-line))
;;       (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

;; (global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
;; (global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)



(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(provide 'init-xterm)