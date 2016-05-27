;;(load-theme 'tango-dark t)
;;(load-theme 'zenburn t)
(if (display-graphic-p) 
    (load-theme 'dracula t) 
  (load-theme 'wheatgrass t))

(provide 'init-themes)
