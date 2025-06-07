;; -*- lexical-binding: t; -*-
(require 'aa/detect-host)
(require 'aa/use-package-presets)

(defvar aa/before-load-theme-hook nil
  "Hooks to run before calling load-theme.")

(advice-add
 'load-theme
 :before
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa/before-load-theme-hook)))

(defvar aa/after-load-theme-hook nil
  "Hooks run after calling load-theme.")

(advice-add
 'load-theme
 :after
 #'(lambda (&rest load-theme-args)
     (run-hooks 'aa/after-load-theme-hook)))

(use-package ef-themes)
(use-package nord-theme)
(use-package solarized-theme)
(use-package gruvbox-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t) )
(use-package base16-theme
  :config
  (setq base16-theme-distinct-fringe-background nil))

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

;; reset themes when switching
(add-hook 'aa/before-load-theme-hook
          #'(lambda nil (mapc #'disable-theme custom-enabled-themes)))

(provide 'aa/ui-colors)
