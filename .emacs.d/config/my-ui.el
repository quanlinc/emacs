;; First take care of UI
;; disable tool-bar/scroll-bar/menu-bar
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Fullscreen mode
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-time)
(setq transient-mark-mode "true")
(setq global-font-lock-mode "true")

;; cursor settings
(set-cursor-color "grey60")

;;Hightlight the current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#3e4446")
(set-face-foreground 'highlight nil)
(set-face-underline-p 'highlight nil)

;; If using Emacs under windows
;; Allows Emacs to recognize cygwin paths
;;(require 'cygwin-mount)
;;(cygwin-mount-activate)

(provide 'my-ui)
