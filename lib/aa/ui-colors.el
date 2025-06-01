;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/use-package-presets)

;; hook to be called on theme reload
(defvar aa/after-load-theme-hook nil
  "Hook run after a color theme is loaded with `load-theme`.")
(defadvice load-theme (after aa/run-after-load-theme-hook activate)
  "Run `aa/after-load-theme-hook`."
  (run-hooks 'aa/after-load-theme-hook))

(use-package ef-themes)
(use-package nord-theme)
(use-package solarized-theme)
(use-package gruvbox-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t) )
(use-package base16-theme)

(cond
 ((aa/work-p) (setq aa/light-theme 'nord
                    aa/dark-theme 'nord))
 (t (setq aa/light-theme 'solarized-dark
                           aa/dark-theme 'base16-tokyodark)))

;; use https://git.sr.ht/~grtcdr/darkman.el
;; to integrate with https://darkman.whynothugo.nl/

(if (and (aa/home-p) (file-exists-p "/usr/bin/darkman"))
    (use-package darkman
      :config
      (setq darkman-themes `(:light ,aa/light-theme :dark ,aa/dark-theme))
      (darkman-mode))
  ;; otherwise fallback to simpler solutions
  (cond
   ((> (decoded-time-hour (decode-time)) 20) (load-theme aa/dark-theme t))
   (t (load-theme aa/light-theme t))))

(provide 'aa/ui-colors)
